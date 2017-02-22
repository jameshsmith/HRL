"use strict"

const Window = require("./window.js")
const tooltip = require("./tooltip.js")

/* Create the inventory grid as an r by 8 table. Returns a <div>
 * containing the <table> element. */
function createGrid (r) {
    var tableContainer = document.createElement("div")
    tableContainer.id = "itemgrid"
    var table = document.createElement("table")
    tableContainer.appendChild(table)
    
    for (var i = 0; i < r; i++) {
	var tr = document.createElement("tr")
	table.appendChild(tr)

	for (var j = 0; j < 8; j++) {
	    var td = document.createElement("td")
	    tr.appendChild(td)
	}
    }
    
    return tableContainer
}

module.exports = function () {
    
    var window = new Window({
	title: "Inventory",
	frameId: "inventory",
	contentId: "items"
    })
    window.onClose(function () {
	window.frame.style.visibility = "hidden"
    })

    var controls = document.createElement("div")
    controls.id = "inventorycontrols"
    var sortButton = document.createElement("img")
    sortButton.src = "ui/isort.png"
    sortButton.addEventListener("mouseover", function () {
	sortButton.src = "ui/isort2.png"
    })
    sortButton.addEventListener("mouseout", function () {
	sortButton.src = "ui/isort.png"
    })
    sortButton.addEventListener("click", function () {
	sort()
    })
    controls.appendChild(sortButton)
    
    window.content.appendChild(controls)

    // uiRows is the total number of rows on the inventory UI
    var uiRows = 1
    
    var grid = createGrid(uiRows)
    window.content.appendChild(grid)

    // items links items to their position in the inventory. imap is a
    // mapping from item names to their positions (of which there may
    // be several). When removing an item from the inventory we use
    // the first location imap points to, so it needs to be sorted as
    // appropriate before drags.
    var items = new Array()
    var imap = {}

    // Returns the first free slot in the items array. Expanding the
    // visual representation of the array as neccessary
    function freeSlot () {
	var free = -1
	
	for (var i = 0; i < items.length && free === -1; i++) {
	    if (items[i] === undefined || items[i] === null) {
		free = i
	    }
	}

	if (free === -1) {
	    free = items.length
	}
	
	// Test is false if we went beyond the maximum space but we
	// still have at least a full free row, if not add a new row.
	if (free >= uiRows * 8 - 8) {
	    var tr = document.createElement("tr")
	    grid.appendChild(tr)
	    
	    for (var j = 0; j < 8; j++) {
		var td = document.createElement("td")
		tr.appendChild(td)
	    }

	    uiRows++
	}

	return free
    }
    this.freeSlot = freeSlot

    this.splitStack = function (loc, number) {
	if (items[loc] !== null && items[loc] !== undefined) {
	    if (items[loc].count > number) {
		items[loc].count -= number
		
		var newLoc = freeSlot()

		items[newLoc] = {
		    name: items[loc].name,
		    count: number
		}
		imap[items[loc].name].push(newLoc)

		refreshAt(loc)
		refreshAt(newLoc)
	    }
	}
    }

    function update (patch) {
	var changes = 0
	var keyDels = 0
	
	for (var i in Object.keys(imap)) {
	    // They object exists in the patch
	    var key = Object.keys(imap)[i - keyDels]

	    if (patch[key] !== undefined) {
		var itemCount = 0
		for (var j = 0; j < imap[key].length; j++) {
		    itemCount += items[imap[key][j]].count
		}
		
		var diff = patch[key] - itemCount
		if (diff > 0) {
		    // The amount of items has increased
		    items[imap[key][0]].count += diff
		    changes++
		} else if (diff < 0) {
		    // The amount of items has decreased
		    var removed = 0
		    
		    for (var j = 0; j < imap[key].length; j++) {
			var c = (items[imap[key][j]].count += diff)
			if (c <= 0) {
			    items[imap[key][j]] = null
			    removed++
			    diff = c
			} else {
			    break
			}
		    }

		    imap[key].splice(0, removed)
		    changes++
		}

		console.log(key + " difference : " + (patch[key] - itemCount))
		delete patch[key]
	    } else {
		// The object doesn't exist in the patch

		for (var j = 0; j < imap[key].length; j++) {
		    items[imap[key][j]] = null
		}
		
		delete imap[key]
		keyDels++
		delete patch[key]
		console.log("Remove all " + key)
		changes++
	    }
	}

	// Everything left in the patch can't exist in the UI
	for (var i in Object.keys(patch)) {
	    key = Object.keys(patch)[i]

	    var loc = freeSlot()
	    items[loc] = {
		name: key,
		count: patch[key]
	    }
	    imap[key] = [loc]
	    
	    console.log("Add : " + key)
	    changes++
	}

	if (changes > 0) {
	    for (var i = 0; i < uiRows * 8; i++) {
		refreshAt(i)
	    }
	}
	console.log("Changes made to inventory: " + changes)
    }
    this.update = update

    function sort () {
	var patch = {}
	
	for (var i in Object.keys(imap)) {
	    var key = Object.keys(imap)[i]

	    var itemCount = 0
	    for (var j = 0; j < imap[key].length; j++) {
		itemCount += items[imap[key][j]].count
	    }

	    patch[key] = itemCount
	}

	update({})
	update(patch)
    }
    this.sort = sort

    function refreshAt (n) {
	var td = document.querySelectorAll("#items td")[n]

	while(td.hasChildNodes()) {
	    td.removeChild(td.lastChild)
	}
	
	if (items[n] !== undefined && items[n] !== null) {
	    var img = document.createElement("img")
	    img.src = "ui/item/" + items[n].name + ".png"
	    var count = document.createTextNode(items[n].count)
	    td.appendChild(img)
	    td.appendChild(count)
	}
    }
}
