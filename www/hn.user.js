// ==UserScript==
// @name        hn
// @namespace   news.ycombinator.com
// @include     https://news.ycombinator.com/item?id=*
// @include     https://news.ycombinator.com/threads?id=*
// @version     1
// @grant       none
// ==/UserScript==

//document.getElementByID();
var comments = document.getElementsByClassName("default")
for (var i = 0; i < comments.length; i++) {
   comments[i].style = "border-left: 1px dashed #333; padding-left: 10px;"
}

// Serif comments.
var fontsiz = '1.2em';
document.querySelectorAll('div.comment').forEach(function(elem){
  elem.style='text-align:justify;padding-right:5px;font-family:Serif;font-size:'+fontsiz});

// Narrower page, on the lefthand side of the window.
var maintab=document.querySelector('table#hnmain');
maintab.width="60%";
document.body.appendChild(maintab);

window.onload=function(){
	var topsel=document.querySelector('.topsel');
	topsel.removeAttribute('class');
}
