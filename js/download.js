
var all_sections = ["linux", "osx", "windows"]

/*

Important selectors:

Here <s> refers to either "linux", "osx" or "windows"

  #<s>-select   - the <li> for selecting the platform

  #<s>-section  - the <section> element for a platform
  #<s>-expander - the expander div for a platform
  #<s>-sidebar  - the div containing the buttons to select a flavor of the distribution
                  e.g. for linux possibilities are "generic", "redhat', "debian", etc.
  "#<s>-sidebar li.flavor-li"
                - selects all of the flavor buttons

  #<s>-content  - the div containing install steps for each flavor
  "#<s>-content div.flavor"
                - selects all the install step divs for the flavors
                  of a platform

*/

// perform an action on all platform sections
function perform_all(action) {
  for (var i = 0; i < all_sections.length; i++) {
    var s = all_sections[i]
    action(s)
  }
}

// perform action on all sections except for sect
function perform_all_except(action, sect) {
  for (var i = 0; i < all_sections.length; i++) {
    var s = all_sections[i]
    if (s != sect) {
      action(s)
    }
  }
}

function collapse_entire_section(sect) {  // added
  $("#" + sect + "-section").hide()
}

function show_sidebar2(sect) {
  $("#" + sect + "-section").show()
  $("#" + sect + "-expander").hide()
  $("#" + sect + "-sidebar").show()
  $("#" + sect + "-content").show()

  // collapse all the other sections
  // perform_all_except( collapse_entire_section, sect )

  if (sect == "windows") {
    // scroll window to the #windows-section
    // $('html, body').animate({ scrollTop: $("#windows-section").offset().top },0)
  }
}

function collapse_section(sect) {
  $("#" + sect + "-expander").show()
  $("#" + sect + "-sidebar").hide()
  $("#" + sect + "-content").hide()
}

function show_sidebar(sect) {
  $("#" + sect + "-expander").hide()
  $("#" + sect + "-sidebar").show()
  $("#" + sect + "-content").show()

  // collapse all the other sections
  perform_all_except( collapse_section, sect )

  if (sect == "windows") {
    // scroll window to the #windows-section
    $('html, body').animate({ scrollTop: $("#windows-section").offset().top },0)
  }
}

function select_flavor(sect, flavor) {
  // flavor is a single word like "generic", "homebrewcask", etc.

  // visit all of the .flavor nodes in the -content div
  // and show only the wanted flavor div hiding others
  var wanted = sect + "-" + flavor

  var target_flavor = flavor

  $("#" + sect + "-content div.flavor").each(function() {
    if (this.id == target_flavor) {
      $(this).show()
    } else {
      $(this).hide()
    }
  })

  // mark the <li> in the flavors div corresponding to this flavor with
  // the "active" class.

  console.log("target_flavor:", target_flavor)
  $("#" + sect + "-sidebar li.flavor-li").each(function() {
    var f = $(this).data("flavor")
    if (f == target_flavor) {
      $(this).addClass("active")
    } else {
      $(this).removeClass("active")
    }
  })

  // make sure all other sections are collapsed
  perform_all_except( collapse_section, sect )

}

function add_expander_action(sect) {
  $("#" + sect + "-expander").click(function() { show_sidebar(sect); return false })
}

function add_flavor_actions(sect) {
  $("#" + sect + "-sidebar .flavor-li a").each(function() {
    $(this).click(function(e) {
                    select_flavor(sect, $(this).data("flavor"))
                  })
  })
}

function select_platform(sect) {
  // select a platform

  // toggle the platform-select buttons
  $(".platform-select").each(function() {
    var plat = $(this).data("platform")
    console.log("--- plat:", plat)
    if (plat == sect) {
      $(this).addClass("active")
      show_sidebar2(plat)
    } else {
      $(this).removeClass("active")
      collapse_entire_section(plat)
    }
  })
}

function add_select_platform_actions(sect) {
  $("#"+sect+"-select a").each(function() {
    $(this).click(function() {
      console.log("--- calling select_platform with:", sect)
      select_platform(sect)
      return false;
    })
  })
}

function initialize() {
  perform_all(add_expander_action)
  perform_all(add_flavor_actions)

  perform_all(add_select_platform_actions)
  $(".bottom-rule").hide()   // hide all of the bottom rules

  // for linux and osx, make the first flavor selected by default
  var sect = "linux"
  $("#" + sect + "-sidebar li.flavor-li").first().addClass("active")
  $("#" + sect + "-content div.flavor").hide()
  $("#" + sect + "-content div.flavor").first().show()

  var sect = "osx"
  $("#" + sect + "-sidebar li.flavor-li").first().addClass("active")
  $("#" + sect + "-content div.flavor").hide()
  $("#" + sect + "-content div.flavor").first().show()

  // collapse all sections
  perform_all(collapse_section)
  $(".platform-toc").show()
  $(".unknown-user-platform").hide()
  $(".found-user-platform").hide()

  // expand the preferred platform
  var loc = window.location.href
  var sect = "linux"
  var basename = loc.split('/').reverse()[0]
  basename = basename.replace(/#.*/,"")
  console.log("basename:", basename)
  if (basename.match(/linux/)) {
    sect = "linux"
  } else if (basename.match(/osx/)) {
    sect = "osx"
  } else if (basename.match(/windows/)) {
    sect = "windows"
  } // else stay with the default

  console.log("selecting platform", sect)
  select_platform(sect)
}

$(document).ready(function() {
  var loc = window.location.href
  if (loc.match(/-nojs.html/)) {
    console.log("--- not running javascript")
    return;
  } else {
    console.log("=== running initialize")
    initialize()
  }
})

