"use strict"

module.exports = function (div, content) {
    var tip = document.createElement("div")
    tip.className = "tooltip"
    tip.innerHTML = content

    div.addEventListener("mouseover", function () {
	var rect = div.getBoundingClientRect()

	tip.style.left = "0px"
	tip.style.top = rect.bottom - rect.top + "px"
	tip.style.animation = "fadein 1s"
	tip.style.visibility = "visible"
    })

    var tipLeave = function () {
	tip.style.removeProperty("animation")
	tip.style.visibility = "hidden"
    }
    
    div.addEventListener("mouseleave", tipLeave)
    tip.addEventListener("mouseenter", tipLeave)
    
    div.appendChild(tip)
}
