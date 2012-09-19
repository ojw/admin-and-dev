var AdminAndDev = {};

AdminAndDev.Room = (function() {
    var look;
    var create;
    var join;
    var leave;
    var send;
    var receive;

    var initCreateButton;
    var initChatWindow;
    var initRoomWindow;
    var initSendButton;

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
        getJson( JSON.stringify( { type: "send"
                                , message: message
                                } )
               , function(data){fn(data)}
               )
    };

    receive = function(fn) {
        getJson( JSON.stringify( { type: "receive" } )
               , fn
               )
    
    };

    displayChat = function(chat) {
        return chat.sender + ": " + chat.message + "<br />"//"\n"
    };

    // displays list backwards
    displayChatList = function(chatList) {
        var rtn = "";
        var len=chatList.length
        for(var i=len-1; i>=0; i--){
            rtn = rtn + displayChat(chatList[i]);
        }
        return rtn;
    };

    initCreateButton = function(str) {
        $(str).click( function(){ Room.create("1")} );
    }

    initSendButton = function(sendButton, chatInput) {
        //$(str).click( function(){ Room.send("FOO")} );
        $(sendButton).click( function(){ 
            var message = $(chatInput).val();
            Room.send(message);
            //alert(message)  
            } );
    }

    initRoomWindow = function(str) {
        var roomWindow = $(str)
        setInterval( function(){ 
                        Room.look( 
                            function(data){
                                roomWindow.html(JSON.stringify(data))
                            } 
                        ) 
                     }
                   , 2000
        )
    }

    initChatWindow = function(str) {
        var chatWindow = $(str)
        setInterval( function(){
                        Room.receive(
                            function(data){
                                var ppData = displayChatList(data);
                                if (ppData !== chatWindow.html()){
                                    chatWindow.html(ppData)
                                }
                            }
                        )
                     }
                   , 1000
        )
    }

    return  { look: look
            , create: create
            , join: join
            , leave: leave
            , send: send
            , receive: receive
            , initCreateButton: initCreateButton
            , initChatWindow: initChatWindow
            , initRoomWindow: initRoomWindow
            , initSendButton: initSendButton
            };

})();
        
var Room = AdminAndDev.Room

$(document).ready( function() {  
        Room.initCreateButton(".create_room_button");
        Room.initChatWindow(".chat_display");
        Room.initRoomWindow(".room_list");
        Room.initSendButton(".chat_send", "input.chat_input");
        
});
