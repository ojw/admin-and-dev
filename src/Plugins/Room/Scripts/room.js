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
    var initRoomWindow;

    var url = "http://localhost:8000/api/room/";

    var getJson;

    getJson = function(input, fn){
        $.ajax( { type: 'POST'
                , url: url
                , data: input
                , success: function(data){fn(data)}
                , dataType: 'json'
                });
    };
        

    look = function(fn) {
        getJson( JSON.stringify({type: "look"})
               , function(data){fn(data)}
               );
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

    receive = function(fn) {
        $.post( url
              , JSON.stringify( { type: "receive" } )
              //, function(data){alert(JSON.stringify(JSON.parse(data)))} //fn
//              , function(data){alert("FOO")} //fn
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

    initRoomWindow = function(str) {
        var roomWindow = $(str)
        setInterval( function(){ 
                        Room.look( 
                            function(data){
                                alert(JSON.stringify(data))
                                roomWindow.html(JSON.stringify(data))
                            } 
                        ) 
                     }
                   , 2000
        )
    }

    initChatWindow = function(str) {
    }

    return  { look: look
            , create: create
            , join: join
            , leave: leave
            , send: send
            , receive: receive
            , initSendButton: initSendButton
            , initChatWindow: initChatWindow
            , initRoomWindow: initRoomWindow
            };

})();
        
var Room = AdminAndDev.Room

$(document).ready( function() {  
        Room.initSendButton(".create_room_button");
//        Room.initChatWindow("foo");
        Room.initRoomWindow("foo");
        
});
