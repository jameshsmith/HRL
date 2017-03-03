import fs = require("fs")

function isHash (chr : string) {
    if (chr === "#") {
        return true
    } else {
        return false
    }
}

// Look at a certain spot in the level
function levelLookAt (camera: THREE.Camera, r: number, c: number) : void {
    let camX = 32 * 41
    let camY = 32 * 41
    let camZ = 32 * 41
    
    camera.position.x = camX
    camera.position.y = camY
    camera.position.z = camZ

    let lX = camX / 4 * 3
    let lZ = camZ / 2

    camera.lookAt(new THREE.Vector3(lX, 0, lZ))
    
    let tX = c * 64 + 32
    let tZ = r * 64 + 32

    camera.position.x -= (lX - tX)
    camera.position.z -= (lZ - tZ)
}

interface MaterialMap {
    [name: string]: THREE.Material
}

interface ObjectMap {
    [name: string]: THREE.Object3D
}

interface Actor {
    row: number
    col: number
    chr: string
}

interface Item {
    row: number
    col: number
    name: string
    count: number
}

interface ActorMap {
    [name: string]: Actor
}

export interface LevelUpdate {
    statics: string[]
    seen: string[]
    visible: string[]
    actors: ActorMap
    messages: string[]
    items: Item[]
}

export class Level {
    public group: THREE.Group = new THREE.Group()

    private visible: THREE.Object3D[][] = new Array(41)
    private seen: THREE.Object3D[][] = new Array(41)
    private actor: ObjectMap = {}
    private items: THREE.Object3D[][] = new Array(41)

    private visibleMats: MaterialMap = {}
    private seenMats: MaterialMap = {}
    private actorMats: MaterialMap = {}
    private itemMats: MaterialMap = {}

    private geoBox = new THREE.BoxGeometry(64, 64, 64)
    private geoFloor = new THREE.PlaneGeometry(64, 64)
    private geoActor = new THREE.PlaneGeometry(64, 64)
    private geoItem = new THREE.PlaneGeometry(32, 32)
    private geoDoor = new THREE.BoxGeometry(64, 64, 8)
    private geoOpenDoor = new THREE.BoxGeometry(8,64,32)

    private camera: THREE.Camera
    private playerLight: THREE.Light

    constructor (camera: THREE.Camera, textureLoader: THREE.TextureLoader, playerLight: THREE.Light) {
        this.group.add(playerLight)
        this.camera = camera
        this.playerLight = playerLight
    
        /* ==================================================
          Structures to hold the levels objects
          ================================================== */
        for (let i = 0; i < 41; i++) {
            this.visible[i] = new Array(41)
        }

        for (let i = 0; i < 41; i++) {
            this.seen[i] = new Array(41)
        }

        for (let i = 0; i < 41; i++) {
            this.items[i] = new Array(41)
        }

        // Make sure the camera is orientated the right way
        levelLookAt(camera, 0, 0)

        /* ==================================================
       Load the materials for the level
       ================================================== */
    
        let files: string[] = fs.readdirSync("textures/static/")
        for (let file of files) {
            console.log(file)
            let tex = textureLoader.load("textures/static/" + file)
            tex.magFilter = THREE.NearestFilter
            tex.minFilter = THREE.NearestFilter

            let visibleMat = new THREE.MeshLambertMaterial({map: tex})
            let seenMat = new THREE.MeshBasicMaterial({map: tex})
            seenMat.color = new THREE.Color(0x202020)
        
            this.visibleMats[file] = visibleMat
            this.seenMats[file] = seenMat
        }

        this.visibleMats['wall.box'] = new THREE.MultiMaterial([
            this.visibleMats['wall.png'],
            this.visibleMats['wall.png'],
            this.visibleMats['top.png'],
            this.visibleMats['top.png'],
            this.visibleMats['wall.png'],
            this.visibleMats['wall.png']
        ])

        this.seenMats['wall.box'] = new THREE.MultiMaterial([
            this.seenMats['wall.png'],
            this.seenMats['wall.png'],
            this.seenMats['top.png'],
            this.seenMats['top.png'],
            this.seenMats['wall.png'],
            this.seenMats['wall.png']
        ])

        this.visibleMats['dooropen.box'] = new THREE.MultiMaterial([
            this.visibleMats['dooropen.png'],
            this.visibleMats['dooropen.png'],
            this.visibleMats['door.png'],
            this.visibleMats['door.png'],
            this.visibleMats['door.png'],
            this.visibleMats['door.png']
        ])

        this.seenMats['dooropen.box'] = new THREE.MultiMaterial([
            this.seenMats['dooropen.png'],
            this.seenMats['dooropen.png'],
            this.seenMats['door.png'],
            this.seenMats['door.png'],
            this.seenMats['door.png'],
            this.seenMats['door.png']
        ])

        this.visibleMats['doorclosed.box'] = new THREE.MultiMaterial([
            this.visibleMats['door.png'],
            this.visibleMats['door.png'],
            this.visibleMats['door.png'],
            this.visibleMats['door.png'],
            this.visibleMats['doorclosed.png'],
            this.visibleMats['doorclosed.png']
        ])

        this.seenMats['doorclosed.box'] = new THREE.MultiMaterial([
            this.seenMats['door.png'],
            this.seenMats['door.png'],
            this.seenMats['door.png'],
            this.seenMats['door.png'],
            this.seenMats['doorclosed.png'],
            this.seenMats['doorclosed.png']
        ])

        files = fs.readdirSync('textures/actor/')
        for (let file of files) {
            console.log(file)
            let tex = textureLoader.load('textures/actor/' + file)
            tex.magFilter = THREE.NearestFilter
            tex.minFilter = THREE.NearestFilter

            let actorMat = new THREE.MeshBasicMaterial({map: tex, transparent: true})
        
            this.actorMats[file] = actorMat
        }

        files = fs.readdirSync('ui/item/')
        for (let file of files) {
            let tex = textureLoader.load("ui/item/" + file)
            tex.magFilter = THREE.NearestFilter
            tex.minFilter = THREE.NearestFilter

            this.itemMats[file] = new THREE.MeshBasicMaterial({map: tex, transparent: true})
        }

        let tex = textureLoader.load('textures/special/pentagram.png')
        tex.magFilter = THREE.NearestFilter
        tex.minFilter = THREE.NearestFilter

        this.visibleMats['pentagram.png'] = new THREE.MeshBasicMaterial({map: tex, transparent: true})
        this.seenMats['pentagram.png'] = new THREE.MeshBasicMaterial({map: tex, transparent: true})

        /* ==================================================
           Load the geometries for the level
           ================================================== */
        this.geoFloor.rotateX(- (Math.PI / 2))
        this.geoFloor.translate(0, -32, 0)
    }

    /* ==================================================
       The static mesher: converts characters into 3D objects
       ================================================== */
    private staticMesher(chr: string, r: number, c: number, ctx: string[], vis: boolean): THREE.Object3D {
        let obj: THREE.Object3D

        let mats
        if (vis) {
            mats = this.visibleMats
        } else {
            mats = this.seenMats
        }

        if (chr === "#") {
            obj = new THREE.Mesh(this.geoBox, mats['wall.box'])
            obj.position.set(64 * c, 0, 64 * r)
            obj.userData.chr = chr
        } else if (chr === " ") {
            obj = new THREE.Mesh(this.geoFloor, mats['floor.png'])
            obj.position.set(64 * c, 0, 64 * r)
            obj.userData.chr = chr
        } else if (chr === "*") {
            let mesh1 = new THREE.Mesh(this.geoFloor, mats['floor.png'])
            mesh1.position.set(64 * c, 0, 64 * r)
            let mesh2 = new THREE.Mesh(this.geoFloor, mats['pentagram.png'])
            mesh2.position.set(64 * c, 0.1, 64 * r)
            mesh2.scale.set(5,1,5)
            mesh2.renderOrder = -1
            let light = new THREE.PointLight(0xAA0000, 2, 300)
            light.position.set(64 * c, 0.2, 64 * r)
            obj = new THREE.Group()
            obj.add(mesh1)
            obj.add(mesh2)
            obj.add(light)
            obj.userData.chr = chr
        } else if (chr === "+") {
            let mesh1 = new THREE.Mesh(this.geoDoor, mats['doorclosed.box'])
            let mesh2 = new THREE.Mesh(this.geoFloor, mats['floor.png'])
            obj = new THREE.Group()
            if (ctx[r - 1].charAt(c) === "#") {
                obj.rotateY(- (Math.PI / 2))
            }
            obj.position.set(64 * c, 0, 64 * r)
            obj.add(mesh1)
            obj.add(mesh2)
            obj.userData.chr = chr
        } else if (chr === "-") {
            let mesh1 = new THREE.Mesh(this.geoOpenDoor, mats['dooropen.box'])
            mesh1.position.set(28, 0, 16)
            let mesh2 = new THREE.Mesh(this.geoOpenDoor, mats['dooropen.box'])
            mesh2.position.set(-28, 0, 16)
            let mesh3 = new THREE.Mesh(this.geoFloor, mats['floor.png'])
            obj = new THREE.Group()
            if (ctx[r - 1].charAt(c) === "#") {
                obj.rotateY(- (Math.PI / 2))
            }
            obj.position.set(64 * c, 0, 64 * r)
            obj.add(mesh1)
            obj.add(mesh2)
            obj.add(mesh3)
            obj.userData.chr = chr
        } else {
            obj = new THREE.Mesh(this.geoFloor, mats['floor.png'])
            obj.position.set(64 * c, 0, 64 * r)
            obj.userData.chr = chr
        }

        return obj
    }

    /* ==================================================
       The actor mesher
       ================================================== */
    private actorMesher(chr: string): THREE.Object3D {
        let mesh

        if (chr === '@') {
            mesh = new THREE.Mesh(this.geoActor, this.actorMats['fighter.png'])
        } else if (chr === 'Z') {
            mesh = new THREE.Mesh(this.geoActor, this.actorMats['zombie.png'])
        } else {
            mesh = new THREE.Mesh(this.geoActor, this.actorMats['corpse.png'])
            mesh.position.y -= 1
        }
        
        mesh.rotation.x = this.camera.rotation.x
        mesh.rotation.y = this.camera.rotation.y
        mesh.rotation.z = this.camera.rotation.z
        mesh.userData.chr = chr
        return mesh
    }

    /* ==================================================
       The item mesher
       ================================================== */
    private itemMesher(name: string, r: number, c: number): THREE.Object3D {
        let mesh = new THREE.Mesh(this.geoItem, this.itemMats[name + ".png"])
        mesh.position.set(64 * c, -16, 64 * r)        
        mesh.rotation.x = this.camera.rotation.x
        mesh.rotation.y = this.camera.rotation.y
        mesh.rotation.z = this.camera.rotation.z
        mesh.userData.name = name

        return mesh
    }

    /* ================================================== 
       Update the level from a JSON message
       ================================================== */
    public update(message: LevelUpdate) {
        // Update the statics
        for (let r = 0; r < 41; r++) {
            for (let c = 0; c < 41; c++) {
                let chr = message.statics[r].charAt(c)
                
                let isSeen = isHash(message.seen[r].charAt(c))      
                let isVisible = isHash(message.visible[r].charAt(c))
                
                let meshV = this.visible[r][c]
                let meshS = this.seen[r][c]
                
                if (meshV === undefined || meshV.userData.chr !== chr) {
                    if (meshV !== undefined) {
                        this.group.remove(meshV)
                        this.group.remove(meshS)
                    }
                    meshV = this.staticMesher(chr, r, c, message.statics, true)
                    this.visible[r][c] = meshV
                    meshS = this.staticMesher(chr, r, c, message.statics, false)
                    this.seen[r][c] = meshS
                    this.group.add(meshV)
                    this.group.add(meshS)
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
        for (let key in message.actors) {
            let act = message.actors[key]

            let meshA = this.actor[key]

            if (meshA === undefined || meshA.userData.chr !== act.chr) {
                console.log("(re)initializing actor: " + key)
                if (meshA !== undefined) {
                    this.group.remove(meshA)
                }
                meshA = this.actorMesher(act.chr)
                this.actor[key] = meshA
                this.group.add(meshA)
            }

            if (meshA.userData.chr === "@") {
                levelLookAt(this.camera, act.row, act.col)
                this.playerLight.position.set(act.col * 64, 32, act.row * 64)
            }
            
            if (isHash(message.visible[act.row].charAt(act.col))) {
                meshA.visible = true
                meshA.position.z = act.row * 64
                meshA.position.x = act.col * 64
            } else {
                meshA.visible = false
            }
        }

        // Update items
        for (let item of message.items) {
            let obj = this.items[item.row][item.col]

            if (obj == null) {
                console.log("Initialising item: " + item.name)
                obj = this.itemMesher(item.name, item.row, item.col)
                this.items[item.row][item.col] = obj
                this.group.add(obj)
            } else if (obj.userData.name != item.name) {
                console.log("Reinitialising item!")
                this.group.remove(obj)
                obj = this.itemMesher(item.name, item.row, item.col)
                this.items[item.row][item.col] = obj
                this.group.add(obj)
            }

            if (isHash(message.visible[item.row].charAt(item.col))) {
                obj.visible = true
            } else {
                console.log("Hiding item " + item.name)
                obj.visible = false
            }
        }

        // Update the message log
        for (let i = message.messages.length - 1; i >= 0; i--) {
            let pNode = document.createElement("p")
            let textNode = document.createTextNode(message.messages[i])
            pNode.appendChild(textNode)
            document.getElementById("messages").appendChild(pNode)
        }

        if (message.messages.length > 0) {
            let messagesNode = document.getElementById("messages")
            messagesNode.scrollTop = messagesNode.scrollHeight  
        }       
    }
    
}
