
function moveStart (event) {
    var dragFrame = event.target.closest(".window")

    var rect = dragFrame.getBoundingClientRect()
    var dragOffX = rect.left - event.pageX
    var dragOffY = rect.top - event.pageY

    function move (event) {
	    dragFrame.style.left = event.pageX + dragOffX + "px"
	    dragFrame.style.top = event.pageY + dragOffY + "px"
    }

    function moveEnd (event) {
	    document.removeEventListener("mousemove", move)
	    document.removeEventListener("mouseup", moveEnd)
    }
    
    document.addEventListener("mousemove", move)
    document.addEventListener("mouseup", moveEnd)
}

export class Frame {
    public frame: HTMLDivElement
    public content: HTMLDivElement

    private closeFrame () : void {
        document.body.removeChild(this.frame)
    }

    public onClose (f) : void {
        this.closeFrame = f
    }

    constructor (settings) {
        /* Create the frame div */
        this.frame = document.createElement("div")
        this.frame.className = "window"

        if (settings.frameId == null) {
	        this.frame.style.height = settings.h + "px"
	        this.frame.style.width = settings.w + "px"
	        this.frame.style.left = settings.x + "px"
	        this.frame.style.top = settings.y + "px"
        } else {
	        this.frame.id = settings.frameId
        }

        /* Create the title bar */
        var tb : HTMLDivElement = document.createElement("div")
        tb.className = "titlebar"
        tb.addEventListener("mousedown", moveStart)

        var tbLeft : HTMLImageElement = document.createElement("img")
        tbLeft.className = "tbleft"
        tbLeft.src = "ui/tbleft.png"
    
        var tbTitle : Text = document.createTextNode(settings.title)
    
        var tbRight : HTMLImageElement = document.createElement("img")
        tbRight.className = "tbright"
        tbRight.src = "ui/tbright.png"

        tbRight.addEventListener("mouseenter", () => {
	        tbRight.src = "ui/tbrightc.png"
        })
        tbRight.addEventListener("mouseleave", () => {
	        tbRight.src = "ui/tbright.png"
        })
        tbRight.addEventListener("mousedown", (event) => {
	        this.closeFrame()
	        event.stopPropagation()
        })
    
        tb.appendChild(tbLeft)
        tb.appendChild(tbTitle)
        tb.appendChild(tbRight)

        /* The content div inside the Frame */
        this.content = document.createElement("div")
        if (settings.contentId != null) {
	        this.content.id = settings.contentId
        }

        /* Add everything to the Frame and add it to the document */
        this.frame.appendChild(tb)
        this.frame.appendChild(this.content)

        document.body.appendChild(this.frame)
    }
}