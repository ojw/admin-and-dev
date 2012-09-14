var AdminAndDev = {};

AdminAndDev.Room = (function() {
    var look;
    var create;
    var join;
    var leave;
    var sent;
    var receive;

    var url = "http://localhost:8000/api/room/";

    look = function() {
        $.post( url
              , JSON.stringify({type: "look"})
              , function(data){}
              )
    };
    create = function(capacity) {
        $.post( url
              , JSON.stringify( { type: "create"
                                , capacity: capacity
                                } )
              , function(data){}
              )
    };
    join = function(roomId) {
        $.post( url
              , JSON.stringify( { type: "join"
                                , roomId: roomId
                                } )
              , function(data){}
              )
    };
    leave = function() {
        $.post( url
              , JSON.stringify( { type: "leave" } )
              , function(data){}
              )
    };
    send = function(message) {
        $.post( url
              , JSON.stringify( { type: "send"
                                , message: message
                                } )
              , function(data){}
              )
    };
    receive = function() {
        $.post( url
              , JSON.stringify( { type: "receive" } )
              , function(data){}
              )
    };
    return  { look: look
            , create: create
            , join: join
            , leave: leave
            , send: send
            , receive: receive
            };
})();
        
var Room = AdminAndDev.Room

$(document).ready( function() {  
        $(".create_room_button").click( function(){ Room.create("1") } );
});
