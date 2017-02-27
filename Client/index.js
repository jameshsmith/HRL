const net = require('net')
const fs = require('fs')
const JsonSocket = require('json-socket')

const {Frame} = require('./frame.js')
const tooltip = require('./tooltip.js')

const Spellbook = require('./spellbook.js')
var spellbook = {}

const Inventory = require('./inventory.js')
var inventory = {}

const Level = require('./level.js')
var level = {}

var scene, camera, renderer;

var rawSocket = new net.Socket()
var socket = new JsonSocket(rawSocket)
socket.connect(3000, 'localhost')

var waiting = false

// When the window is closed, end the connection.
document.addEventListener('unload', function(event) {
    socket.end();
});

socket.on('connect', function() {
    console.log('connected to server!')
})

socket.on('message', updateMsg)

function updateMsg (message) {
    if (message.type === "level") {
        console.log(message.payload)
        level.update(message.payload)
    } else if (message.type === "loading") {
        console.log("Loading: " + message.payload)
    }

    waiting = false
}

function actionMessage (act) {
    waiting = true
    
    return {"type": "action", "payload": act}
}

var chord = ""

function keyPress (event) {
    if (!waiting) {
        keyPressPrime(event)
    }
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
        } else if (event.key === "b") {
            var win = document.getElementById("spellbook")
            win.style.visibility = "visible"
        } else if (event.key === "m") {
            var win = document.getElementById("messagebox")
            win.style.visibility = "visible"
        } else if (event.key === "i") {
            var win = document.getElementById("inventory")
            win.style.visibility = "visible"
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

function start () {
    initTHREE()
    initUI()
    animate()
}

function initUI () {
    var messages = new Frame({
        title: "Messages",
        frameId: "messagebox",
        contentId: "messages"
    })
    messages.onClose(function () {
        messages.frame.style.visibility = "hidden"
    })

    spellbook = new Spellbook()
    inventory = new Inventory()
}

function initTHREE() {
    scene = new THREE.Scene();
 
    var width = window.innerWidth;
    var height = window.innerHeight;
    camera = new THREE.OrthographicCamera(- (width / 2), width / 2, height / 2, - (height / 2), 1, 10000);

    var textureLoader = new THREE.TextureLoader()
    
    var ambLight = new THREE.AmbientLight(0x505050);
    scene.add(ambLight);
    
    playerLight = new THREE.PointLight(0xFFCC66, 1.5, 600)
    
    level = new Level(camera, textureLoader, playerLight)

    var texSelection = textureLoader.load('textures/ui/select.png')
    texSelection.magFilter = THREE.NearestFilter
    texSelection.minFilter = THREE.NearestFilter

    var matSelection = new THREE.MeshBasicMaterial({map: texSelection, transparent: true})
    
    var geoSelection = new THREE.PlaneGeometry(64, 64)
    geoSelection.rotateX(- (Math.PI / 2))
    geoSelection.translate(0, -31, 0)
    selection = new THREE.Mesh(geoSelection, matSelection)

    scene.add(selection)
    scene.add(level.group)
    
    renderer = new THREE.WebGLRenderer()
    renderer.setSize(window.innerWidth, window.innerHeight)
    renderer.domElement.id = "canvas"

    renderer.domElement.addEventListener("mousemove", canvasMouseMove)
    renderer.domElement.addEventListener("click", canvasClick)
    
    document.body.appendChild(renderer.domElement)
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
        socket.sendMessage(actionMessage({"name": spellbook.spell(), "row": selectedR, "col": selectedC}))
    }
}

function animate() {
    requestAnimationFrame(animate);

    camera.position.x += 0
    camera.position.z += 0

    var d = new Date()

    playerLight.intensity = 1 + Math.sin(d.getTime() / 500) / 5
    
    renderer.render(scene, camera)
}
