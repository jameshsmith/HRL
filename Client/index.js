const net = require('net')
const fs = require('fs')
const JsonSocket = require('json-socket')

var scene, camera, renderer;
var geometry, material, mesh;

var staticVisibleMats = {}
var staticSeenMats = {}

var rawSocket = new net.Socket()
var socket = new JsonSocket(rawSocket)
socket.connect(3000, 'localhost')

var waiting = false

// When the window is closed, send a FIN packet.
document.addEventListener('unload', function(event) {
    socket.end();
});


// Setting up the level
var levelGroup = new THREE.Group()

var levelVisible = new Array(41)
for (var i = 0; i < 41; i++) {
    levelVisible[i] = new Array(41)
}

var levelSeen = new Array(41)
for (var i = 0; i < 41; i++) {
    levelSeen[i] = new Array(41)
}

var levelActor = {}

socket.on('connect', function() {
    console.log('connected to server!')
})

socket.on('message', updateMsg)

function isHash (chr) {
    if (chr === '#') {
	return true
    } else {
	return false
    }
}

function chrActorMesh (chr) {
    var mesh
    
    if (chr === '@') {
	mesh = new THREE.Mesh(geoActor, fighterMat)
    } else if (chr === 'Z') {
	mesh = new THREE.Mesh(geoActor, zombieMat)
    } else {
	mesh = new THREE.Mesh(geoActor, corpseMat)
	mesh.position.y -= 1
    }
	
    mesh.rotation.x = camera.rotation.x
    mesh.rotation.y = camera.rotation.y
    mesh.rotation.z = camera.rotation.z
    mesh.userData.chr = chr
    return mesh
}

// chrMesh :: Char -> Row -> Col -> Bool -> UArray (Row, Col) Char -> Object3D
function chrMesh (chr, r, c, ctx, vis) {
    var obj

    var mats
    if (vis) {
	mats = staticVisibleMats
    } else {
	mats = staticSeenMats
    }

    if (chr === '#') {
	obj = new THREE.Mesh(geometry, mats['wall.box'])
	obj.position.set(64 * c, 0, 64 * r)
	obj.userData.chr = chr
    } else if (chr === ' ') {
	obj = new THREE.Mesh(geoFloor, mats['floor.png'])
	obj.position.set(64 * c, 0, 64 * r)
	obj.userData.chr = chr
    } else if (chr === '+') {
	var mesh1 = new THREE.Mesh(geoDoor, mats['doorclosed.box'])
	var mesh2 = new THREE.Mesh(geoFloor, mats['floor.png'])
	obj = new THREE.Group()
	if (ctx[r - 1].charAt(c) === '#') {
	    obj.rotateY(- (Math.PI / 2))
	}
	obj.position.set(64 * c, 0, 64 * r)
	obj.add(mesh1, mesh2)
	obj.userData.chr = chr
    } else if (chr === '-') {
	var mesh1 = new THREE.Mesh(geoOpenDoor, mats['dooropen.box'])
	mesh1.position.set(28, 0, 16)
	var mesh2 = new THREE.Mesh(geoOpenDoor, mats['dooropen.box'])
	mesh2.position.set(-28, 0, 16)
	mesh3 = new THREE.Mesh(geoFloor, mats['floor.png'])
	obj = new THREE.Group()
	if (ctx[r - 1].charAt(c) === '#') {
	    obj.rotateY(- (Math.PI / 2))
	}
	obj.position.set(64 * c, 0, 64 * r)
	obj.add(mesh1, mesh2, mesh3)
	obj.userData.chr = chr
    } else {
	obj = new THREE.Mesh(geoFloor, mats['floor.png'])
	obj.position.set(64 * c, 0, 64 * r)
	obj.userData.chr = chr
    }

    return obj
}

function updateMsg (message) {
    if (message.type === "level") {
	updateLevel(message.payload)
    } else if (message.type === "loading") {
	console.log("Loading: " + message.payload)
    }

    waiting = false
}

function updateLevel (message) {
    // console.log(message)
    
    // Update statics
    for (var r = 0; r < 41; r++) {
	for (var c = 0; c < 41; c++) {
	    var chr = message.statics[r].charAt(c)
	    
	    var isSeen = isHash(message.seen[r].charAt(c))	    
	    var isVisible = isHash(message.visible[r].charAt(c))
	    
	    var meshV = levelVisible[r][c]
	    var meshS = levelSeen[r][c]
	    
	    if (meshV === undefined || meshV.userData.chr !== chr) {
		//console.log("Initializing new static for R: " + r + " C: " + c)
		if (meshV !== undefined) {
		    levelGroup.remove(meshV, meshS)
		}
		meshV = chrMesh(chr, r, c, message.statics, true)
		levelVisible[r][c] = meshV
		meshS = chrMesh(chr, r, c, message.statics, false)
		levelSeen[r][c] = meshS
		levelGroup.add(meshV, meshS)
		//console.log("CHILDREN: " + levelGroup.children.length)
	    }

	    if (isVisible) {
		meshV.visible = true
		meshS.visible = false
	    } else if (isSeen) {
		meshV.visible = false
		meshS.visible = true
	    } else {
		meshV.visible = false
		meshS.visible = false
	    }
	}
    }

    // Update actors
    for (var key in message.actors) {
	var act = message.actors[key]

	var meshA = levelActor[key]

	if (meshA === undefined || meshA.userData.chr !== act.chr) {
	    console.log('(re)initializing actor: ' + key)
	    if (meshA !== undefined) {
		levelGroup.remove(meshA)
	    }
	    meshA = chrActorMesh(act.chr)
	    levelActor[key] = meshA
	    levelGroup.add(meshA)
	}

	if (meshA.userData.chr === '@') {
	    camLookAt(act.row, act.col)
	}
	
	if (isHash(message.visible[act.row].charAt(act.col))) {
	    meshA.visible = true
	    meshA.position.z = act.row * 64
	    meshA.position.x = act.col * 64
	} else {
	    meshA.visible = false
	}
    }

    for (var i = message.messages.length - 1; i >= 0; i--) {
	var pNode = document.createElement("p")
	var textNode = document.createTextNode(message.messages[i])
	pNode.appendChild(textNode)
	document.getElementById("messages").appendChild(pNode)
    }

    if (message.messages.length > 0) {
	var messagesNode = document.getElementById("messages")
	messagesNode.scrollTop = messagesNode.scrollHeight	
    }
}

function start () {
    init();
    animate();
}

function actionMessage (act) {
    return {"type": "action", "payload": act}
}

var chord = ""

function keyPress (event) {
    if (!waiting) {
	keyPressPrime(event)
    }
    waiting = true
}

function keyPressPrime (event) {

    if (chord === "") {
	if (event.key === "w") {
	    socket.sendMessage(actionMessage({"move": "N"}))
	} else if (event.key === "d") {
	    socket.sendMessage(actionMessage({"move": "E"}))
	} else if (event.key === "s") {
	    socket.sendMessage(actionMessage({"move": "S"}))	
	} else if (event.key === "a") {
	    socket.sendMessage(actionMessage({"move": "W"}))
	} else if (event.key === " ") {
	    socket.sendMessage(actionMessage("skip"))
	} else if (event.key === "t") {
	    chord += "t"
	}
    } else if (chord === "t") {
	if (event.key === "w") {
	    socket.sendMessage(actionMessage({"activate": "N"}))
	} else if (event.key === "d") {
	    socket.sendMessage(actionMessage({"activate": "E"}))
	} else if (event.key === "s") {
	    socket.sendMessage(actionMessage({"activate": "S"}))	
	} else if (event.key === "a") {
	    socket.sendMessage(actionMessage({"activate": "W"}))
	}

	chord = ""
    }
}

// =====================================================
// Dragging Windows
// =====================================================
var dragWindow

function tbMove (event) {
    dragWindow.style.left = event.pageX + dragWindow.dragOffX + "px"
    dragWindow.style.top = event.pageY + dragWindow.dragOffY + "px"
}

function tbMoveStart (event) {
    dragWindow = event.target.parentNode
    var rect = dragWindow.getBoundingClientRect()
    
    dragWindow.dragOffX = rect.left - event.pageX
    dragWindow.dragOffY = rect.top - event.pageY
    
    document.addEventListener("mousemove", tbMove)
}

function tbMoveEnd (event) {
    document.removeEventListener("mousemove", tbMove)
}


function start () {
    init();
    animate();
}

function camLookAt (r, c) {
    var camX = 32 * 41
    var camY = 32 * 41
    var camZ = 32 * 41
    
    camera.position.x = camX
    camera.position.y = camY
    camera.position.z = camZ

    var lX = camX / 4 * 3
    var lZ = camZ / 2

    camera.lookAt(new THREE.Vector3(lX, 0, lZ))
    
    var tX = c * 64 + 32
    var tZ = r * 64 + 32

    camera.position.x -= (lX - tX)
    camera.position.z -= (lZ - tZ)
}

function init() {
 
    scene = new THREE.Scene();
 
    width = window.innerWidth;
    height = window.innerHeight;
    camera = new THREE.OrthographicCamera(- (width / 2), width / 2, height / 2, - (height / 2), 1, 10000);

    camLookAt(0, 0)

    textureLoader = new THREE.TextureLoader()

    texFighter = textureLoader.load('textures/actor/fighter.png')
    texFighter.magFilter = THREE.NearestFilter
    texFighter.minFilter = THREE.NearestFilter
    texZombie = textureLoader.load('textures/actor/zombie.png')
    texZombie.magFilter = THREE.NearestFilter
    texZombie.minFilter = THREE.NearestFilter
    texSelect = textureLoader.load('textures/ui/select.png')
    texSelect.magFilter = THREE.NearestFilter
    texSelect.minFilter = THREE.NearestFilter
    texCorpse = textureLoader.load('textures/actor/corpse.png')
    texCorpse.magFilter = THREE.NearestFilter
    texCorpse.minFilter = THREE.NearestFilter
    
    var files = fs.readdirSync('textures/static/')
    for (var i in files) {
	console.log(files[i])
	var tex = textureLoader.load('textures/static/' + files[i])
	tex.magFilter = THREE.NearestFilter
	tex.minFilter = THREE.NearestFilter

	var visibleMat = new THREE.MeshBasicMaterial({map: tex})
	var seenMat = new THREE.MeshBasicMaterial({map: tex})
	seenMat.color = new THREE.Color(0x303030)
	
	staticVisibleMats[files[i]] = visibleMat
	staticSeenMats[files[i]] = seenMat
    }

    staticVisibleMats['wall.box'] = new THREE.MultiMaterial([
	staticVisibleMats['wall.png'],
	staticVisibleMats['wall.png'],
	staticVisibleMats['top.png'],
	staticVisibleMats['top.png'],
	staticVisibleMats['wall.png'],
	staticVisibleMats['wall.png']
    ])

    staticSeenMats['wall.box'] = new THREE.MultiMaterial([
	staticSeenMats['wall.png'],
	staticSeenMats['wall.png'],
	staticSeenMats['top.png'],
	staticSeenMats['top.png'],
	staticSeenMats['wall.png'],
	staticSeenMats['wall.png']
    ])

    staticVisibleMats['dooropen.box'] = new THREE.MultiMaterial([
	staticVisibleMats['dooropen.png'],
	staticVisibleMats['dooropen.png'],
	staticVisibleMats['door.png'],
	staticVisibleMats['door.png'],
	staticVisibleMats['door.png'],
	staticVisibleMats['door.png']
    ])

    staticSeenMats['dooropen.box'] = new THREE.MultiMaterial([
	staticSeenMats['dooropen.png'],
	staticSeenMats['dooropen.png'],
	staticSeenMats['door.png'],
	staticSeenMats['door.png'],
	staticSeenMats['door.png'],
	staticSeenMats['door.png']
    ])

    staticVisibleMats['doorclosed.box'] = new THREE.MultiMaterial([
	staticVisibleMats['door.png'],
	staticVisibleMats['door.png'],
	staticVisibleMats['door.png'],
	staticVisibleMats['door.png'],
	staticVisibleMats['doorclosed.png'],
	staticVisibleMats['doorclosed.png']
    ])

    staticSeenMats['doorclosed.box'] = new THREE.MultiMaterial([
	staticSeenMats['door.png'],
	staticSeenMats['door.png'],
	staticSeenMats['door.png'],
	staticSeenMats['door.png'],
	staticSeenMats['doorclosed.png'],
	staticSeenMats['doorclosed.png']
    ])
    
    fighterMat = new THREE.MeshBasicMaterial({map: texFighter, transparent: true})
    zombieMat = new THREE.MeshBasicMaterial({map: texZombie, transparent: true})
    selectMat = new THREE.MeshBasicMaterial({map: texSelect, transparent: true})
    corpseMat = new THREE.MeshBasicMaterial({map: texCorpse, transparent: true})
    
    geometry = new THREE.BoxGeometry(64, 64, 64)

    geoFloor = new THREE.PlaneGeometry(64, 64)
    geoFloor.rotateX(- (Math.PI / 2))
    geoFloor.translate(0, -32, 0)
    
    geoActor = new THREE.PlaneGeometry(64, 64)

    geoDoor = new THREE.BoxGeometry(64, 64, 8)

    geoOpenDoor = new THREE.BoxGeometry(8,64,32)

    geoSelection = new THREE.PlaneGeometry(64, 64)
    geoSelection.rotateX(- (Math.PI / 2))
    geoSelection.translate(0, -31, 0)
    selection = new THREE.Mesh(geoSelection, selectMat)

    scene.add(selection);
    scene.add(levelGroup);
 
    renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    renderer.domElement.id = "canvas"

    renderer.domElement.addEventListener("mousemove", canvasMouseMove)
    renderer.domElement.addEventListener("click", canvasClick)
    
    document.body.appendChild( renderer.domElement )

    populateSpellbook()
}

var raycaster = new THREE.Raycaster()
var mouseXY = new THREE.Vector2()
var groundPlane = new THREE.Plane(new THREE.Vector3(0, 1, 0), 32)

var selectedR = 0
var selectedC = 0

function canvasMouseMove (event) {
    mouseXY.x = (event.pageX / 1680) * 2 - 1
    mouseXY.y = - (event.pageY / 1050) * 2 + 1

    raycaster.setFromCamera(mouseXY, camera)

    var v3 = raycaster.ray.intersectPlane(groundPlane)

    selectedR = Math.ceil(v3.z / 64)
    selectedC = Math.ceil(v3.x / 64)
    
    selection.position.x = selectedC * 64
    selection.position.z = selectedR * 64
}

function canvasClick (event) {
    if (!waiting) {
	socket.sendMessage(actionMessage({"name": spell, "row": selectedR, "col": selectedC}))
    }
    waiting = true
}

function animate() {
 
    requestAnimationFrame(animate);

    camera.position.x += 0
    camera.position.z += 0
    //camera.rotateX(- 0.002)
    /*
    group.rotation.x += 0.02;
    group.rotation.y += 0.01;
    */

    renderer.render(scene, camera);
 
}

// =====================================================
// Spell Management
// =====================================================

var spell = ""
var spellbook = ["Curse of Misfortune", "Fireblast"]

var spellSrc
var spellAlt
var spellOrigin = null

function populateSpellbook () {
    for (var s in spellbook) {
	var entry = document.createElement("div")
	entry.className = "bookentry"

	var img = document.createElement("img")
	img.src = "ui/spell/" + spellbook[s] + ".png"
	img.alt = spellbook[s]
	img.draggable = true
	img.addEventListener("dragstart", spellDrag)
	img.addEventListener("dragend", spellDragEnd)

	var name = document.createElement("div")
	name.className = "name"
	name.innerHTML = spellbook[s]

	entry.appendChild(img)
	entry.appendChild(name)
	
	var spellbookNode = document.getElementById("spells")
	spellbookNode.appendChild(entry)
    }
}

function allowDrop (event) {
    event.preventDefault()
}

function spellDrag (event) {
    
    var img = event.target

    event.dataTransfer.setDragImage(img, 16, 16)

    spellSrc = img.src
    spellAlt = img.alt
    spellOrigin = img.closest(".spellbar")

    var slots = document.getElementsByClassName("spellbar");

    for (var i = 0; i < slots.length; i++) {
	slots[i].style.background = "limegreen"
	slots[i].addEventListener("dragover", allowDrop)
	slots[i].addEventListener("drop", spellDrop)
    }
}

function spellDragEnd (event) {
    event.preventDefault()

    var slots = document.getElementsByClassName("spellbar")

    for (var i = 0; i < slots.length; i++) {
	slots[i].style.removeProperty("background")
	slots[i].removeEventListener("dragover", allowDrop)
	slots[i].removeEventListener("drop", spellDrop)
    }
}
    
function spellDrop (event) {
    event.preventDefault()

    var target = event.target.closest(".spellbar")

    if (spellOrigin !== null) {
	while (spellOrigin.hasChildNodes()) {
	    spellOrigin.removeChild(spellOrigin.lastChild)
	}

	if (target.hasChildNodes()) {
	    spellOrigin.appendChild(target.lastChild)
	}
    }
    
    while (target.hasChildNodes()) {
	console.log("Removing " + target.lastChild)
	
	target.removeChild(target.lastChild)
    }
    
    var img = document.createElement("img")
    img.src = spellSrc
    img.alt = spellAlt
    img.draggable = true
    img.addEventListener("click", spellClick)
    img.addEventListener("dragstart", spellDrag)
    img.addEventListener("dragend", spellDragEnd)
    
    target.appendChild(img)
}

function spellClick (event) {
    if (event.target.alt !== undefined) {
	console.log("Setting spell to " + event.target.alt)
	spell = event.target.alt
    }
}
