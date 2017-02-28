import {Frame} from "./frame.js"

export class Spellbook extends Frame {
    public spell: string = "Time Reversal"
    private knownSpells: string[] = ["Curse of Misfortune", "Fireblast", "Time Reversal"]

    constructor() {
        super({
            title: "Spellbook",
            frameId: "spellbook",
            contentId: "spells"
        })
        this.onClose(() => this.container.style.visibility = "hidden")

        this.populate()
    }

    public sort (): void {
        this.knownSpells.sort()
        this.populate()
    }

    public populate(): void {
        while (this.content.hasChildNodes()) {            
            this.content.removeChild(this.content.lastChild)
        }
        
        for (let spell of this.knownSpells) {
            let entry = document.createElement("div")
            entry.className = "bookentry"
            
            let img = document.createElement("img")
            img.src = "ui/spell/" + spell + ".png"
            img.alt = spell

            let name = document.createElement("div")
            name.className = "name"
            name.innerHTML = spell

            entry.appendChild(img)
            entry.appendChild(name)
            
            this.content.appendChild(entry)
        }
    }

}

