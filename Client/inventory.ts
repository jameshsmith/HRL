import {Frame} from "./frame"

/* Create the inventory grid as an r by 8 table. Returns a <div>
 * containing the <table> element. */
function createGrid (r: number): HTMLDivElement {
    let tableContainer = document.createElement("div")
    tableContainer.id = "itemgrid"
    let table = document.createElement("table")
    tableContainer.appendChild(table)
    
    for (let i = 0; i < r; i++) {
        let tr = document.createElement("tr")
        table.appendChild(tr)

        for (let j = 0; j < 8; j++) {
            let td = document.createElement("td")
            tr.appendChild(td)
        }
    }
    
    return tableContainer
}

interface Slot {
    name: string
    count: number
}

interface SlotMap {
    [name: string]: number[]
}

export interface InventoryUpdate {
    [name: string]: number
}

export class Inventory extends Frame {
    private controls: HTMLDivElement = document.createElement("div")
    private sortButton: HTMLImageElement = document.createElement("img")

    // uiRows is the number of rows on the inventory UI
    private uiRows: number = 1
    private grid = createGrid(this.uiRows)
    private table: HTMLTableElement

    // items links items to their position in the inventory. imap is a
    // mapping from item names to their positions (of which there may
    // be several). When removing an item from the inventory we use
    // the first location imap points to, so it needs to be sorted as
    // appropriate before drags.
    private items: Slot[] = new Array()
    private imap: SlotMap = {}

    constructor() {
        super({
            title: "Inventory",
            frameId: "inventory",
            contentId: "items"
        })

        this.table = <HTMLTableElement>this.grid.firstChild

        // Set up the window
        this.onClose(() => this.container.style.visibility = "hidden")

        this.controls.id = "inventorycontrols"
        this.sortButton.src = "ui/isort.png"
        this.sortButton.addEventListener("mouseover", () => {
            this.sortButton.src = "ui/isort2.png"
        })
        this.sortButton.addEventListener("mouseout", () => {
            this.sortButton.src = "ui/isort.png"
        })
        this.sortButton.addEventListener("click", () => {
            this.sort()
        })
        this.controls.appendChild(this.sortButton)
        this.content.appendChild(this.controls)
        this.content.appendChild(this.grid)
    }

    public freeSlot(): number {
        let free = -1
        
        for (let i = 0; i < this.items.length && free === -1; i++) {
            if (this.items[i] == null) {
                free = i
            }
        }

        if (free === -1) {
            free = this.items.length
        }
        
        // Test is false if we went beyond the maximum space but we
        // still have at least a full free row, if not add a new row.
        if (free >= this.uiRows * 8 - 8) {
            let tr = document.createElement("tr")
            this.table.appendChild(tr)
            
            for (let j = 0; j < 8; j++) {
                let td = document.createElement("td")
                tr.appendChild(td)
            }

            this.uiRows++
        }

        return free
    }

    private refreshAt(loc: number): void {
        let td = document.querySelectorAll("#items td")[loc]

        while(td.hasChildNodes()) {
            td.removeChild(td.lastChild)
        }
        
        if (this.items[loc] != null) {
            let img = document.createElement("img")
            img.src = "ui/item/" + this.items[loc].name + ".png"
            let count = document.createTextNode(this.items[loc].count.toString())
            let span = document.createElement("span")
            span.appendChild(count)
            td.appendChild(img)
            td.appendChild(span)
        }
    }

    public splitStack(loc: number, number: number): void {
        if (this.items[loc] != null) {
            if (this.items[loc].count > number) {
                this.items[loc].count -= number
                
                let newLoc = this.freeSlot()

                this.items[newLoc] = {
                    name: this.items[loc].name,
                    count: number
                }
                this.imap[this.items[loc].name].push(newLoc)

                this.refreshAt(loc)
                this.refreshAt(newLoc)
            }
        }
    }

    public update(patch: InventoryUpdate): void {
        let changes = 0
        
        for (let key of Object.keys(this.imap)) {

            if (patch[key] != null) {
                let itemCount = 0
                for (let i = 0; i < this.imap[key].length; i++) {
                    itemCount += this.items[this.imap[key][i]].count
                }
                
                let diff = patch[key] - itemCount
                if (diff > 0) {
                    // The amount of items has increased
                    this.items[this.imap[key][0]].count += diff
                    changes++
                } else if (diff < 0) {
                    // The amount of items has decreased
                    let removed = 0
                    
                    for (let i = 0; i < this.imap[key].length; i++) {
                        let c = (this.items[this.imap[key][i]].count += diff)
                        if (c <= 0) {
                            this.items[this.imap[key][i]] = null
                            removed++
                            diff = c
                        } else {
                            break
                        }
                    }

                    this.imap[key].splice(0, removed)
                    changes++
                }

                console.log(key + " difference : " + (patch[key] - itemCount))
                delete patch[key]
            } else {
                // The object doesn't exist in the patch

                for (let i = 0; i < this.imap[key].length; i++) {
                    this.items[this.imap[key][i]] = null
                }
                
                delete this.imap[key]
                delete patch[key]
                console.log("Remove all " + key)
                changes++
            }
        }

        // Everything left in the patch can't exist in the UI
        for (let key of Object.keys(patch)) {
            let loc = this.freeSlot()
            this.items[loc] = {
                name: key,
                count: patch[key]
            }
            this.imap[key] = [loc]
            
            console.log("Add : " + key)
            changes++
        }

        if (changes > 0) {
            for (let i = 0; i < this.uiRows * 8; i++) {
                this.refreshAt(i)
            }
        }
        console.log("Changes made to inventory: " + changes)
        console.log(this.items.length)
    }

    // FIXME: This doesn't currently sort, it just restacks,
    // as the keys are not in a sorted order.
    public sort(): void {
        let patch: InventoryUpdate = {}
        
        for (let key of Object.keys(this.imap)) {
            let itemCount = 0
            for (let i = 0; i < this.imap[key].length; i++) {
                itemCount += this.items[this.imap[key][i]].count
            }

            patch[key] = itemCount
        }

        this.update({})
        this.update(patch)
    }
}
