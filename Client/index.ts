import * as electron from "electron"

import net = require("net")
import fs = require("fs")
import JsonSocket = require("json-socket")

import {Frame} from "./frame"
import {Inventory} from "./inventory"
import {Spellbook} from "./spellbook"
import {Level, LevelUpdate} from "./level"

exports = {}

let spellbook: Spellbook
let inventory: Inventory
let level: Level

let scene: THREE.Scene
let camera: THREE.Camera
let renderer: THREE.Renderer

let chord: string = ""

let rawSocket: net.Socket = new net.Socket()
let socket = new JsonSocket(rawSocket)
socket.connect(3000, "localhost")

let waiting: boolean = false

// When the window is closed, end the connection.
document.addEventListener('unload', function(event) {
    socket.end();
});

socket.on("connect", () => {
    console.log("connected to server!")
})

socket.on("message", updateMessage)

function start(): void {
    initUI()
    initTHREE()
    animate()
}

interface Message {
    type: string
    payload: any
}

function updateMessage(message: Message): void {
    if (message.type === "level") {
        console.log(message.payload)
        level.update(<LevelUpdate>message.payload)
    } else if (message.type === "loading") {
        console.log("Loading: " + message.payload)
    }

    waiting = false
}

function actionMessage(act: any): Message {
    waiting = true
    
    return {"type": "action", "payload": act}
}

function keyPress(event: KeyboardEvent): void {
    if (!waiting) {
        handleKeyPress(event)
    }
}

function handleKeyPress(event: KeyboardEvent): void {

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
            let win = document.getElementById("spellbook")
            win.style.visibility = "visible"
        } else if (event.key === "m") {
            let win = document.getElementById("messagebox")
            win.style.visibility = "visible"
        } else if (event.key === "i") {
            let win = document.getElementById("inventory")
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

function initUI (): void {
    let messages = new Frame({
        title: "Messages",
        frameId: "messagebox",
        contentId: "messages"
    })
    messages.onClose(function () {
        messages.container.style.visibility = "hidden"
    })

    spellbook = new Spellbook()
    inventory = new Inventory()
}

let selection: THREE.Mesh
let playerLight: THREE.PointLight

function initTHREE(): void {
    scene = new THREE.Scene();
 
    let width = window.innerWidth;
    let height = window.innerHeight;
    camera = new THREE.OrthographicCamera(- (width / 2), width / 2, height / 2, - (height / 2), 1, 10000);

    let textureLoader = new THREE.TextureLoader()
    
    let ambLight = new THREE.AmbientLight(0x505050);
    scene.add(ambLight);
    
    playerLight = new THREE.PointLight(0xFFCC66, 1.5, 600)
    
    level = new Level(camera, textureLoader, playerLight)

    let texSelection = textureLoader.load("textures/ui/select.png")
    texSelection.magFilter = THREE.NearestFilter
    texSelection.minFilter = THREE.NearestFilter

    let matSelection = new THREE.MeshBasicMaterial({map: texSelection, transparent: true})
    
    let geoSelection = new THREE.PlaneGeometry(64, 64)
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

let raycaster = new THREE.Raycaster()
let mouseXY = new THREE.Vector2()
let groundPlane = new THREE.Plane(new THREE.Vector3(0, 1, 0), 32)

let selectedR = 0
let selectedC = 0

function canvasMouseMove(event: MouseEvent): void {
    mouseXY.x = (event.pageX / 1680) * 2 - 1
    mouseXY.y = - (event.pageY / 1050) * 2 + 1

    raycaster.setFromCamera(mouseXY, camera)

    let v3 = raycaster.ray.intersectPlane(groundPlane)

    selectedR = Math.ceil(v3.z / 64)
    selectedC = Math.ceil(v3.x / 64)
    
    selection.position.x = selectedC * 64
    selection.position.z = selectedR * 64
}

function canvasClick(event: MouseEvent): void {
    if (!waiting) {
        socket.sendMessage(actionMessage({"name": spellbook.spell, "row": selectedR, "col": selectedC}))
    }
}

function animate(): void {
    requestAnimationFrame(animate);

    camera.position.x += 0
    camera.position.z += 0

    let d = new Date()

    playerLight.intensity = 1 + Math.sin(d.getTime() / 500) / 5
    
    renderer.render(scene, camera)
}
