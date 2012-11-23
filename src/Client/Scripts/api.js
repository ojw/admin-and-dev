var AdminAndDev = {};

AdminAndDev.API = {};
AdminAndDev.Client = {};

AdminAndDev.Test = (function() {
    return  { test: function(){ alert("FOO"); }
            , foop: 2
            };
           
})();

AdminAndDev.Internal = (function() {

    var url = "http://localhost:8000/api/";
    var getJson;

    getJson = function(input, fn){
        $.ajax( { type: 'POST'
                , url: url
                , data: JSON.stringify(input)
                , success: fn
                , dataType: 'json'
                });
    };

    return { getJson: getJson };

})();
        
AdminAndDev.API.Room = (function() {

    var send;
    var receive;

    send = function(message, fn) {
        AdminAndDev.Internal.getJson( 
                { domain: "room"
                , type: "send"
                , message: message
                }
                , fn
                );
    };

    receive = function(fn) {
        AdminAndDev.Internal.getJson( 
                { domain: "room"
                , type: "receive" 
                }
                , fn
                );
    
    };

    return  { send: send
            , receive: receive
            };

})();

AdminAndDev.API.Lobby = (function() {

        var join;
        var leave;
        var look;

        join = function(lobby, fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "lobby"
                        , type: "join"
                        , lobby: "leave"
                        }
                        , fn
                        );
        };

        leave = function(fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "lobby"
                        , type: "leave"
                        }
                        , fn
                        );
        };

        look = function(fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "lobby"
                        , type: "look"
                        }
                        , fn
                        );
        };

        return  { join: join
                , leave: leave
                , look: look
                };
})();

AdminAndDev.API.Matchmaker = (function() {

        var create;
        var join;
        var leave;
        var look;

        create = function(capacity, required, fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "matchmaker"
                        , type: "create"
                        , capacity: capacity
                        , required: required
                        }
                        , fn
                        );
        };

        join = function(matchmaker, fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "matchmaker"
                        , type: "join"
                        , matchmaker: matchmaker
                        }
                        , fn
                        );
        };

        leave = function(fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "matchmaker"
                        , type: "leave"
                        }
                        , fn
                        );
        };

        look = function(fn) {
                AdminAndDev.Internal.getJson(
                        { domain: "matchmaker"
                        , type: "look"
                        }
                        , fn
                        );
        };

})();

AdminAndDev.Client.Room = (function() {

    var initSendButton;
    var initChatWindow;
    var displayChat;
    var displayChatList;

    var chatRefreshDelay = 500
    
    displayChat = function(chat) {
        return chat.sender + ": " + chat.message + "\n"
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

    initSendButton = function(sendButton, chatInput) {
        $(chatInput).focus( function() {this.select()}  );
        $(sendButton).click( function(){ 
            var message = $(chatInput).val();
            AdminAndDev.API.Room.send(message);
            $(chatInput).val("");
            $(chatInput).focus();
            return false;
            } );
    }

    initChatWindow = function(str) {
        var chatWindow = $(str)
        setInterval( function(){
                        AdminAndDev.API.Room.receive(
                            function(data){
                                var ppData = displayChatList(data);
                                if (ppData !== chatWindow.val()){
                                    chatWindow.val(ppData);
                                    chatWindow.scrollTop( chatWindow[0].scrollHeight );
                                }
                            }
                        );
                     }
                   , chatRefreshDelay
        )
    }

    return  { initChatWindow: initChatWindow
            , initSendButton: initSendButton
            }
})();

$(document).ready( function() {  
        AdminAndDev.Client.Room.initChatWindow(".chat_display");
        AdminAndDev.Client.Room.initSendButton(".chat_send", "input.chat_input");
}); 
