// ==UserScript==
// @name        reddit
// @namespace   reddit.com
// @include     https://www.reddit.com/*
// @include     https://reddit.com/*
// @version     1
// @grant       none
// ==/UserScript==


/// Gimme my overview the way I like it and fuck you:
document.querySelector('span.user > a').href += 'overview/';
document.querySelectorAll('a.author').forEach(function(item){item.href+='/overview/';});

/// Sidebar toggling:

var gk_sidebar = document.getElementsByClassName('side')[0];
var gk_hbr = document.getElementById("header-bottom-right");

gk_sidebar.hidden = true;

gk_hbr.innerHTML += "<span class='separator'>|</span>";
gk_hbr.innerHTML += "<span class='user'>\
<a href='javascript:(function(){var s=document.getElementsByClassName(\"side\")[0];s.hidden=!s.hidden;}())'>sidebar</span>\
</span>";

/// Kill some annoyances:
window.onload = function () {
	document.getElementsByClassName("subscribebar")[0].hidden = true;
	document.getElementsByClassName("infobar")[0].hidden = true;
};
