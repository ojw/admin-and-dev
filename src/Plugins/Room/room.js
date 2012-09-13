$(document).ready( function() {  
        $(".create_room_button").click( function(){ ajaxCreateRoom() } );
});


var ajaxCreateRoom = function() {
        $.post( "http://localhost:8000/api/room/create", JSON.stringify({ type: "create", capacity: "1" }), function(data) {alert(data)})
};

var ajaxLookRooms = function() {
        $.post( "http://localhost:8000/api/room/look", 
                        JSON.stringify({ type: "look" }),
                        function(data) {
                                alert(data)
                        }
              )
};
/*
var domCreateRoom = function(roomName) {
        var rm = $("<div></div>");
        rm.attr('class', 'chat_room');
        return(rm[0]);
};
*/

