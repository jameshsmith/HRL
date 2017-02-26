
moveStart = (event) ->
    console.log "moving"
    
    dragWindow = event.target.closest ".window"

    rect = dragWindow.getBoundingClientRect()
    dragOffX = rect.left - event.pageX
    dragOffY = rect.top - event.pageY

    move = (event) ->
        dragWindow.style.left = "#{event.pageX + dragOffX}px"
        dragWindow.style.top = "#{event.pageY + dragOffY}px"

    moveEnd = (event) ->
        document.removeEventListener "mousemove", move
        document.removeEventListener "mouseup", moveEnd

    document.addEventListener "mousemove", move
    document.addEventListener "mouseup", moveEnd
    
module.exports = (settings) ->
    win = document.createElement "div"
    win.className = "window"

    if not settings.frameId?
        win.style.height = "#{settings.h}px"
        win.style.width = "#{settings.w}px"
        win.style.left = "#{settings.x}px"
        win.style.top = "#{settings.y}px"
    else
        win.id = settings.frameId

    tb = document.createElement "div"
    tb.className = "titlebar"
    tb.addEventListener "mousedown", moveStart

    tbLeft = document.createElement "img"
    tbLeft.className = "tbleft"
    tbLeft.src = "ui/tbleft.png"

    tbTitle = document.createTextNode settings.title

    tbRight = document.createElement "img"
    tbRight.className = "tbright"
    tbRight.src = "ui/tbright.png"

    closeWindow = -> document.body.removeChild win
    @onClose = (f) -> closeWindow = f

    tbRight.addEventListener "mouseenter", -> tbRight.src = "ui/tbrightc.png"
    tbRight.addEventListener "mouseleave", -> tbRight.src = "ui/tbright.png"
    tbRight.addEventListener "mousedown", (event) ->
        closeWindow
        event.stopPropagation

    tb.appendChild child for child in [tbLeft, tbTitle, tbRight]

    @content = document.createElement "div"

    @content.id = settings.contentId if settings.contentId?

    win.appendChild tb
    win.appendChild @content

    @frame = win
    document.body.appendChild win

    @
