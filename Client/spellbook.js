const {Frame} = require('./frame.js')
const tooltip = require('./tooltip.js')

module.exports = function () {
    var spell = ""
    var knownSpells = ["Curse of Misfortune", "Fireblast", "Time Reversal"]

    var spellSrc
    var spellAlt
    var spellOrigin = null

    var window = new Frame({
        title: "Spellbook",
        frameId: "spellbook",
        contentId: "spells"
    })
    window.onClose(function () {
        window.frame.style.visibility = "hidden"
    })
    
    this.spell = function () {
        return spell
    }

    this.reverseSort = function () {
        knownSpells.sort()
        knownSpells.reverse()
        this.populate()
    }

    this.sort = function () {
        knownSpells.sort()
        this.populate()
    }
    
    this.populate = function () {
        while (window.content.hasChildNodes()) {            
            window.content.removeChild(window.content.lastChild)
        }
        
        for (var s in knownSpells) {
            var entry = document.createElement("div")
            entry.className = "bookentry"
            tooltip(entry, "This is a spell")
            
            var img = document.createElement("img")
            img.src = "ui/spell/" + knownSpells[s] + ".png"
            img.alt = knownSpells[s]
            img.draggable = true
            img.addEventListener("dragstart", spellDrag)
            img.addEventListener("dragend", spellDragEnd)
            img.addEventListener("click", spellClick)
            
            tooltip(img, "This is a spell")
            
            var name = document.createElement("div")
            name.className = "name"
            name.innerHTML = knownSpells[s]

            entry.appendChild(img)
            entry.appendChild(name)
            
            window.content.appendChild(entry)
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
            spell = event.target.alt
        }
    }

    this.populate()
}
