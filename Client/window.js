"use strict"

function tbMoveStart (event) {
    var dragWindow = event.target.closest(".window")

    var rect = dragWindow.getBoundingClientRect()
    var dragOffX = rect.left - event.pageX
    var dragOffY = rect.top - event.pageY

    var tbMove = function (event) {
	dragWindow.style.left = event.pageX + dragOffX + "px"
	dragWindow.style.top = event.pageY + dragOffY + "px"
    }

    var tbMoveEnd = function (event) {
	document.removeEventListener("mousemove", tbMove)
	document.removeEventListener("mouseup", tbMoveEnd)
    }
    
    document.addEventListener("mousemove", tbMove)
    document.addEventListener("mouseup", tbMoveEnd)
}

module.exports = function(settings) {
    /* Create the frame div */
    var win = document.createElement("div")
    win.className = "window"

    if (settings.frameId === undefined) {
	win.style.height = settings.h + "px"
	win.style.width = settings.w + "px"
	win.style.left = settings.x + "px"
	win.style.top = settings.y + "px"
    } else {
	win.id = settings.frameId
    }

    /* Create the title bar */
    var tb = document.createElement("div")
    tb.className = "titlebar"
    tb.addEventListener("mousedown", tbMoveStart)

    var tbLeft = document.createElement("img")
    tbLeft.className = "tbleft"
    tbLeft.src = "ui/tbleft.png"
    
    var tbTitle = document.createTextNode(settings.title)
    
    var tbRight = document.createElement("img")
    tbRight.className = "tbright"
    tbRight.src = "ui/tbright.png"

    /* Handle closing the window */
    var closeWindow = function () {
	document.body.removeChild(win)
    }
    this.onClose = function (f) {
	closeWindow = f
    }
    tbRight.addEventListener("mouseenter", function () {
	tbRight.src = "ui/tbrightc.png"
    })
    tbRight.addEventListener("mouseleave", function () {
	tbRight.src = "ui/tbright.png"
    })
    tbRight.addEventListener("mousedown", function (event) {
	closeWindow()
	event.stopPropagation()
    })
    
    tb.appendChild(tbLeft)
    tb.appendChild(tbTitle)
    tb.appendChild(tbRight)

    /* The content div inside the window */
    this.content = document.createElement("div")
    if (settings.contentId !== undefined) {
	this.content.id = settings.contentId
    }

    /* Add everything to the window and add it to the document */
    win.appendChild(tb)
    win.appendChild(this.content)

    this.frame = win
    document.body.appendChild(win)
}
