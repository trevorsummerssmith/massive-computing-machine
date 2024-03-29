var SERVER_URL = "http://localhost:8080";

var DRAWN_TILE_WIDTH = 10;
var DRAWN_TILE_HEIGHT = 10;
var TILE_BORDER_WIDTH = 1;
var TILE_BORDER_HEIGHT = 1;
var TILE_WIDTH = DRAWN_TILE_WIDTH - TILE_BORDER_WIDTH;
var TILE_HEIGHT = DRAWN_TILE_HEIGHT - TILE_BORDER_HEIGHT;

// Global variables
var game;

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
    console.log("Received from ajax: " + jqXHR);
    return draw_board(jqXHR.responseJSON); // this shouldn't draw the board
}

function canvas_to_tile_coords(x, y) {
    var tile_x = Math.floor(x / DRAWN_TILE_WIDTH);
    var tile_y = Math.floor(y / DRAWN_TILE_HEIGHT);
    return [tile_x, tile_y];
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
    var character = BOARD['cells'][index];
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
	name: BOX_LAYER_PREFIX + index, // Note: name cannot be an integer
	fillStyle: "#ff00ff",
	x: coords[0], y: coords[1],
	width: TILE_WIDTH, height: TILE_HEIGHT,
	fromCenter: false, // always use top left coords
	//click: function(layer) { callback_get_name(layer.name); }
    });
}

function draw_special_character(tx, ty) {
    var coords = tile_to_canvas_coords(tx, ty);
    console.log("Drawing special character");
    $("#canvas-world").drawPolygon({
	fillStyle: "#FFAD00",
	strokeWidth: 0,
	sides: 5,
	concavity: 0.5,
	radius: 8,
	x: coords[0]-4, y: coords[1]-3,
	fromCenter: false
    });
}

function draw_empty_tile(tx, ty) {
    var coords = tile_to_canvas_coords(tx, ty);
    console.log("Drawing empty tile");
    // XXX TODO This should probably call 'clear'?
    $("#canvas-world").drawRect({
	fillStyle: "#ffffff",
	x: coords[0], y: coords[1],
	width: TILE_WIDTH, height: TILE_HEIGHT,
	fromCenter: false // always use top left coords
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

    // Add the grid lines
    $('#canvas-world').draw({
	fn: function(ctx) {
            ctx.strokeStyle = '#ccc';
	    for (var i=0; i < 1000; i+= 10) {
		// Horizontal lines
		ctx.moveTo(i,0);
		ctx.lineTo(i,1000);

		// Vertical lines
		ctx.moveTo(0,i);
		ctx.lineTo(1000,i);
	    }
	    // Note: It is important the stroke call is here, at the end
	    // of all the the ctx lineTo cmds. It is extremely faster to
	    // treat the lineTos as a single polyline under the hood.
	    ctx.stroke();
	}
    });

    // Draw characters
    console.log(board);
    var characters = board['cells'];
    $.each(characters, function (index, value) {
	// Get the place and draw it.
	var tx = value['x'];
	var ty = value['y'];
	var type = value['t'];
	if (type == "Hole") {
	    draw_hole(tx, ty);
	}
	else if (type == "Hill") {
	    draw_hill(tx, ty);
	}
	else if (type == "Food") {
	    draw_food(tx, ty);
	}
	else if (type == "hunter") {
	    // FindFood Character
	    draw_special_character(tx, ty);
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

var TILE_TYPES = {
    EMPTY_TILE : 0,
    HOLE : 1,
    HILL : 2,
    FOOD : 3,
    CHARACTER : 4
};

function tile_to_str(tile) {
    switch(tile) {
    case TILE_TYPES.EMPTY_TILE:
	return "EmptyTile";
    case TILE_TYPES.HOLE:
	return "Hole";
    case TILE_TYPES.HILL:
	return "Hill";
    case TILE_TYPES.FOOD:
	return "Food";
    case TILE_TYPES.CHARACTER:
	return "Character";
    default:
	console.log("This should never happen");
    }
};

var CURRENT_TILE_TYPE = TILE_TYPES.EMPTY_TILE;

function set_tile_type(type) {
    console.log("Setting current tile type to: " + type);
    CURRENT_TILE_TYPE = type;
}

function edit_mode() {
    // Make sure we're not playing the game.
    clearInterval(PLAY_INTERVAL_TIMER);
    PLAY_INTERVAL_TIMER = PLAY_INTERVAL_TIMER_NOT_SET;

    // Show the stuff.
    $("#edit_controls").css({"visibility": "visible"});

    // Hide the other stuff.
    // TODO

    // And layer that will get events
    $("#canvas-world").drawRect({
	layer: true,
	fillStyle: 'rgba(100, 0,0, 0.2)',
	x: 0,
	y: 0,
	width: 2000,
	height: 2000,
	click: function (layer) {
	    var x = layer.eventX;
	    var y = layer.eventY;
	    var tileCoords = canvas_to_tile_coords(x, y);
	    var tileX = tileCoords[0];
	    var tileY = tileCoords[1];

	    console.log("Adding: " + tile_to_str(CURRENT_TILE_TYPE) + " at " +
			"x: " + x + " y: " + y +
			" tileX: " + tileX + " tileY: " + tileY);

	    // Now that we're done -- redraw
	    var cell = {'x':tileX, 'y':tileY, 't' : tile_to_str(CURRENT_TILE_TYPE)};
	    BOARD['cells'].push(cell);
	    draw_board(BOARD);
	}
    });
}

function cb_save_board() {
    var json = JSON.stringify(BOARD);
    console.log("Sending board to server...")
    console.log(json);
    var jqXHR = $.ajax({
	dataType: "json",
	url: SERVER_URL + "/save-game",
	data: json,
	type: 'POST',
	async: false
    });
    // Return should be {'filename': name}
    alert("Saved board to: '" + jqXHR.responseJSON['filename'] + "'");
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
