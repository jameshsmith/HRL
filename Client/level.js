"use strict"

function isHash (chr) {
    if (chr === '#') {
	return true
    } else {
	return false
    }
}

// Look at a certain spot in the level
function levelLookAt (camera, r, c) {
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

module.exports = function (camera, textureLoader, playerLight) {
    this.group = new THREE.Group()

    this.group.add(playerLight)
    
    /* ==================================================
       Structures to hold the levels objects
       ================================================== */
    
    var visible = new Array(41)
    for (var i = 0; i < 41; i++) {
	visible[i] = new Array(41)
    }

    var seen = new Array(41)
    for (var i = 0; i < 41; i++) {
	seen[i] = new Array(41)
    }

    var actor = {}

    // Make sure the camera is orientated the right way
    levelLookAt(camera, 0, 0)

    /* ==================================================
       Load the materials for the level
       ================================================== */
    var visibleMats = {}
    var seenMats = {}
    var actorMats = {}
    
    var files = fs.readdirSync('textures/static/')
    for (var i in files) {
	console.log(files[i])
	var tex = textureLoader.load('textures/static/' + files[i])
	tex.magFilter = THREE.NearestFilter
	tex.minFilter = THREE.NearestFilter

	var visibleMat = new THREE.MeshLambertMaterial({map: tex})
	var seenMat = new THREE.MeshBasicMaterial({map: tex})
	seenMat.color = new THREE.Color(0x202020)
	
	visibleMats[files[i]] = visibleMat
	seenMats[files[i]] = seenMat
    }

    visibleMats['wall.box'] = new THREE.MultiMaterial([
	visibleMats['wall.png'],
	visibleMats['wall.png'],
	visibleMats['top.png'],
	visibleMats['top.png'],
	visibleMats['wall.png'],
	visibleMats['wall.png']
    ])

    seenMats['wall.box'] = new THREE.MultiMaterial([
	seenMats['wall.png'],
	seenMats['wall.png'],
	seenMats['top.png'],
	seenMats['top.png'],
	seenMats['wall.png'],
	seenMats['wall.png']
    ])

    visibleMats['dooropen.box'] = new THREE.MultiMaterial([
	visibleMats['dooropen.png'],
	visibleMats['dooropen.png'],
	visibleMats['door.png'],
	visibleMats['door.png'],
	visibleMats['door.png'],
	visibleMats['door.png']
    ])

    seenMats['dooropen.box'] = new THREE.MultiMaterial([
	seenMats['dooropen.png'],
	seenMats['dooropen.png'],
	seenMats['door.png'],
	seenMats['door.png'],
	seenMats['door.png'],
	seenMats['door.png']
    ])

    visibleMats['doorclosed.box'] = new THREE.MultiMaterial([
	visibleMats['door.png'],
	visibleMats['door.png'],
	visibleMats['door.png'],
	visibleMats['door.png'],
	visibleMats['doorclosed.png'],
	visibleMats['doorclosed.png']
    ])

    seenMats['doorclosed.box'] = new THREE.MultiMaterial([
	seenMats['door.png'],
	seenMats['door.png'],
	seenMats['door.png'],
	seenMats['door.png'],
	seenMats['doorclosed.png'],
	seenMats['doorclosed.png']
    ])

    files = fs.readdirSync('textures/actor/')
    for (var i in files) {
	console.log(files[i])
	var tex = textureLoader.load('textures/actor/' + files[i])
	tex.magFilter = THREE.NearestFilter
	tex.minFilter = THREE.NearestFilter

	var actorMat = new THREE.MeshBasicMaterial({map: tex, transparent: true})
	
	actorMats[files[i]] = actorMat
    }

    var tex = textureLoader.load('textures/special/pentagram.png')
    tex.magFilter = THREE.NearestFilter
    tex.minFilter = THREE.NearestFilter

    visibleMats['pentagram.png'] = new THREE.MeshBasicMaterial({map: tex, transparent: true})
    seenMats['pentagram.png'] = new THREE.MeshBasicMaterial({map: tex, transparent: true})

    /* ==================================================
       Load the geometries for the level
       ================================================== */
    var geoBox = new THREE.BoxGeometry(64, 64, 64)
    var geoFloor = new THREE.PlaneGeometry(64, 64)
    geoFloor.rotateX(- (Math.PI / 2))
    geoFloor.translate(0, -32, 0)
    var geoActor = new THREE.PlaneGeometry(64, 64)
    var geoDoor = new THREE.BoxGeometry(64, 64, 8)
    var geoOpenDoor = new THREE.BoxGeometry(8,64,32)

    /* ==================================================
       The static mesher: converts characters into 3D objects
       ================================================== */
    function staticMesher (chr, r, c, ctx, vis) {
	var obj

	var mats
	if (vis) {
	    mats = visibleMats
	} else {
	    mats = seenMats
	}

	if (chr === '#') {
	    obj = new THREE.Mesh(geoBox, mats['wall.box'])
	    obj.position.set(64 * c, 0, 64 * r)
	    obj.userData.chr = chr
	} else if (chr === ' ') {
	    obj = new THREE.Mesh(geoFloor, mats['floor.png'])
	    obj.position.set(64 * c, 0, 64 * r)
	    obj.userData.chr = chr
	} else if (chr === '*') {
	    var mesh1 = new THREE.Mesh(geoFloor, mats['floor.png'])
	    mesh1.position.set(64 * c, 0, 64 * r)
	    var mesh2 = new THREE.Mesh(geoFloor, mats['pentagram.png'])
	    mesh2.position.set(64 * c, 0.1, 64 * r)
	    mesh2.scale.set(5,1,5)
	    mesh2.renderOrder = -1
	    var light = new THREE.PointLight(0xAA0000, 2, 300)
	    light.position.set(64 * c, 0.2, 64 * r)
	    obj = new THREE.Group()
	    obj.add(mesh1, mesh2, light)
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
	    var mesh3 = new THREE.Mesh(geoFloor, mats['floor.png'])
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

    /* ==================================================
       The actor mesher
       ================================================== */
    function actorMesher (chr) {
	var mesh
    
	if (chr === '@') {
	    mesh = new THREE.Mesh(geoActor, actorMats['fighter.png'])
	} else if (chr === 'Z') {
	    mesh = new THREE.Mesh(geoActor, actorMats['zombie.png'])
	} else {
	    mesh = new THREE.Mesh(geoActor, actorMats['corpse.png'])
	    mesh.position.y -= 1
	}
	
	mesh.rotation.x = camera.rotation.x
	mesh.rotation.y = camera.rotation.y
	mesh.rotation.z = camera.rotation.z
	mesh.userData.chr = chr
	return mesh
    }
    
    /* ================================================== 
       Update the level from a JSON message
       ================================================== */
    this.update = function (message) {
	// Update the statics
	for (var r = 0; r < 41; r++) {
	    for (var c = 0; c < 41; c++) {
		var chr = message.statics[r].charAt(c)
		
		var isSeen = isHash(message.seen[r].charAt(c))	    
		var isVisible = isHash(message.visible[r].charAt(c))
		
		var meshV = visible[r][c]
		var meshS = seen[r][c]
		
		if (meshV === undefined || meshV.userData.chr !== chr) {
		    if (meshV !== undefined) {
			this.group.remove(meshV, meshS)
		    }
		    meshV = staticMesher(chr, r, c, message.statics, true)
		    visible[r][c] = meshV
		    meshS = staticMesher(chr, r, c, message.statics, false)
		    seen[r][c] = meshS
		    this.group.add(meshV, meshS)
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

	    var meshA = actor[key]

	    if (meshA === undefined || meshA.userData.chr !== act.chr) {
		console.log('(re)initializing actor: ' + key)
		if (meshA !== undefined) {
		    this.group.remove(meshA)
		}
		meshA = actorMesher(act.chr)
		actor[key] = meshA
		this.group.add(meshA)
	    }

	    if (meshA.userData.chr === '@') {
		levelLookAt(camera, act.row, act.col)
		playerLight.position.set(act.col * 64, 32, act.row * 64)
	    }
	    
	    if (isHash(message.visible[act.row].charAt(act.col))) {
		meshA.visible = true
		meshA.position.z = act.row * 64
		meshA.position.x = act.col * 64
	    } else {
		meshA.visible = false
	    }
	}

	// Update the message log
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
    
}
