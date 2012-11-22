var AdminAndDev = {};

AdminAndDev.Text = (function() {
    return  { test: function(){ alert("FOO"); }
            , foop: 2
            };
           
})();

AdminAndDev.Room = (function() {

    var send;
    var receive;

    var initSendButton;
    var initChatWindow;

    var chatRefreshDelay = 500
    var url = "http://localhost:8000/api/";

    var getJson;

    getJson = function(input, fn){
        $.ajax( { type: 'POST'
                , url: url
                , data: input
                , success: function(data){fn(data)}
                , dataType: 'json'
                });
    };
        
    send = function(message) {
        getJson( JSON.stringify( { domain: "room"
                                 , type: "send"
                                 , message: message
                                 } )
               , function(data){fn(data)}
               )
    };

    receive = function(fn) {
        getJson( JSON.stringify( { domain: "room"
                                 , type: "receive" } )
               , fn
               )
    
    };

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
            Room.send(message);
            $(chatInput).val("");
            $(chatInput).focus();
            return false;
            } );
    }

    initChatWindow = function(str) {
        var chatWindow = $(str)
        setInterval( function(){
                        Room.receive(
                            function(data){
                                var ppData = displayChatList(data);
                                if (ppData !== chatWindow.val()){
                                    chatWindow.val(ppData);
                                    chatWindow.scrollTop( chatWindow[0].scrollHeight );
                                }
                            }
                        )
                     }
                   , chatRefreshDelay
        )
    }

    return  { send: send
            , receive: receive
            , initChatWindow: initChatWindow
            , initSendButton: initSendButton
            };

})();
        
var Room = AdminAndDev.Room

$(document).ready( function() {  
        Room.initChatWindow(".chat_display");
        Room.initSendButton(".chat_send", "input.chat_input");
});
