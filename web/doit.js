var SERVER_URL = "http://localhost:8080";

var DRAWN_TILE_WIDTH = 10;
var DRAWN_TILE_HEIGHT = 10;
var TILE_BORDER_WIDTH = 1;
var TILE_BORDER_HEIGHT = 1;
var TILE_WIDTH = DRAWN_TILE_WIDTH - TILE_BORDER_WIDTH;
var TILE_HEIGHT = DRAWN_TILE_HEIGHT - TILE_BORDER_HEIGHT;

function get_game() {
    var jqXHR = $.ajax({
	dataType: "json",
	url: SERVER_URL + '/game', async : false
    });
    return jqXHR.responseJSON;
}

function advance_game () {
    var jqXHR = $.ajax({
	dataType: "json",
	url: SERVER_URL + "/advance-game", async: false
    });
    BOARD = jqXHR.responseJSON; // XXX
    return draw_board(jqXHR.responseJSON); // this shouldn't draw the board
}

function tile_to_canvas_coords(tile_x, tile_y) {
    // Map tile array access to canvas coordinates
    // Currently a tile takes up 9pxx9px
    var map_x = (tile_x * DRAWN_TILE_WIDTH) + TILE_BORDER_WIDTH;
    var map_y = (tile_y * DRAWN_TILE_HEIGHT) + TILE_BORDER_HEIGHT;
    return [map_x, map_y];
}

var BOARD = 0;

var BOX_LAYER_PREFIX = "box_";

function callback_get_name(name) {
    // layer is the layer object name --
    // a string "box_" + 
    var index = name.substring(4, name.length);

    // now we have an index!
    var character = BOARD['board'][index];
    console.log(name);
    console.log(index)
    console.log(character);
    $("#info-place").html("Character: " + character[2] +
			  " (" + character[0] + ", " + character[1] + ")");
}

function draw_character(tx,ty,index) {
    // from 0,0 as top left
    var coords = tile_to_canvas_coords(tx, ty);
    console.log("Drawing character: " + "tx: " + tx + " ty: " + ty + " x: " + coords[0] + "y: " + coords[1]);
    $("#canvas-world").drawRect({
	layer: true,
	name: BOX_LAYER_PREFIX + index, // Note: name cannot be an integer
	fillStyle: "#ff00ff",
	x: coords[0], y: coords[1],
	width: TILE_WIDTH, height: TILE_HEIGHT,
	fromCenter: false, // always use top left coords
	click: function(layer) { callback_get_name(layer.name); }
    });
}

function draw_hole(tx, ty) {
    var coords = tile_to_canvas_coords(tx, ty);
    console.log("Drawing hole");
    $("#canvas-world").drawEllipse({
	fillStyle: '#000',
	x: coords[0], y: coords[1],
	width: 10, height: 10,
	fromCenter: false
    });
}

function draw_hill(tx, ty) {
    var coords = tile_to_canvas_coords(tx, ty);
    console.log("Drawing hill");
    $("#canvas-world").drawPolygon({
	fillStyle: "#7F2626",
	strokeWidth: 0,
	sides: 3,
	radius: 7,
	x: coords[0]-2, y: coords[1]-1,
	fromCenter: false
    });
}

function draw_food(tx,ty) {
    var coords = tile_to_canvas_coords(tx, ty);
    console.log("Drawing food");
    $("#canvas-world").drawRect({
	fillStyle: "#00ff00",
	x: coords[0], y: coords[1],
	width: TILE_WIDTH, height: TILE_HEIGHT,
	fromCenter: false
    });
}

function draw_board(board) {
    // Empty it out
    $('#canvas-world').clearCanvas();

    // board object as made from the json response
    // Draw a 10px by 10px grid
    // boxes are 9px by 9px, then grid
    for (var i=0; i < 1000; i+= 10) {
	$("#canvas-world").drawLine({
	    strokeWidth: 1,
	    strokeStyle: '#ccc',
	    x1: 0, y1: i,
	    x2: 1000, y2: i
	});
    }

    for (var i=0; i < 1000; i+= 10) {
	$("#canvas-world").drawLine({
	    strokeWidth: 1,
	    strokeStyle: '#ccc',
	    x1: i, y1: 0,
	    x2: i, y2: 1000
	});
    }

    // Draw characters
    var characters = board['board'];
    $.each(characters, function (index, value) {
	// Get the place and draw it.
	var tx = value[0];
	var ty = value[1];
	// name is value[2]
	var type = value[2];
	if (type == "Hole") {
	    draw_hole(tx, ty);
	}
	else if (type == "Hill") {
	    draw_hill(tx, ty);
	}
	else if (type == "Food") {
	    draw_food(tx, ty);
	}
	else {
	    // Character. Bad for now. Need to redo this.
	    draw_character(tx, ty, index);
	}
    });
}

function start() {
    var board = get_game();
    BOARD = board; // XXX
    draw_board(board);
}


var PLAY_INTERVAL_TIMER_NOT_SET = -1;
var PLAY_INTERVAL_TIMER = PLAY_INTERVAL_TIMER_NOT_SET;

function play_pause() {
    if (PLAY_INTERVAL_TIMER == PLAY_INTERVAL_TIMER_NOT_SET) {
	// It is not playing, so play it
	PLAY_INTERVAL_TIMER = setInterval(function() { advance_game(); }, 500);
    } else {
	clearInterval(PLAY_INTERVAL_TIMER);
	PLAY_INTERVAL_TIMER = PLAY_INTERVAL_TIMER_NOT_SET;
    }    
}

/** Keyboard Shortcuts
  for future reference 'a' is 65
*/

// 'p' - pause/play
$(document).keydown(function(event) {
    if (event.which == 80) {
	play_pause();
    }
});
