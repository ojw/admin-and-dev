var com.adminanddev.room

com.adminanddev.room = function() {
    var look
    ,   create
    ,   join
    ,   leave
    ,   sent
    ,   receive
    ;
    var url = "http://localhost:8000/api/room/"
    look = function() {
        $.post( url + "look"
              , JSON.stringify({type: "look"})
              , function(data){}
              )
    }
    create = function(capacity) {
        $.post( url + "create
              , JSON.stringify( { type: "create"
                                , capacity: capacity
                                } )
              , function(data){}
              )
    }
    join = function(roomId) {
        $.post( url + "join"
              , JSON.stringify( { type: "join"
                                , roomId: roomId
                                } )
              , function(data){}
              )
    }
    leave = function() {
        $.post( url + "leave"
              , JSON.stringify( { type: "leave" } )
              , function(data){}
              )
    }
    send = function(message) {
        $.post( url + "send"
              , JSON.stringify( { type: "send"
                                , message: message
                                } )
              , function(data){}
              )
    }
    receive = function() {
        $.post( url + "receive"
              , JSON.stringify( { type: "send" } )
              , function(data){}
              )
    }
}
        
        


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

