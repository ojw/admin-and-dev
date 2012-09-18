var AdminAndDev = {};

AdminAndDev.Room = (function() {
    var look;
    var create;
    var join;
    var leave;
    var send;
    var receive;

    var initSendButton;
    var initChatWindow;

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

    displayChat = function(chat) {
        return chat.sender + ": " + chat.message + "\n"
    };

    displayChatList = function(chatList) {
        var rtn = "";
        var length=chatList.length()
        for(var i=0; i<length; i++){
            rtn = rtn + displayChat(chatList[i]);
        }
        return rtn;
    };

    initSendButton = function(str) {
        $(str).click( function(){ Room.create("1")} );
    }

    initChatWindow = function(str) {
        //setInterval( "alert('Hello')", 500 );
    }

    return  { look: look
            , create: create
            , join: join
            , leave: leave
            , send: send
            , receive: receive
            , initSendButton: initSendButton
            , initChatWindow: initChatWindow
            };

})();
        
var Room = AdminAndDev.Room

$(document).ready( function() {  
        Room.initSendButton(".create_room_button");
        Room.initChatWindow("foo");
        
});
