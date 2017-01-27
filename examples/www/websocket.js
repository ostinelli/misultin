    
	if (!window.WebSocket && window.MozWebSocket)
		window.WebSocket=window.MozWebSocket;


    var client = {
        connect: function(){
            this._ws=new WebSocket("ws://192.168.0.3:8000/static");
            this._ws.onopen=this._onopen;
            this._ws.onmessage=this._onmessage;
            this._ws.onclose=this._onclose;
        },
    
        _onopen: function(){
			var erlang = $$(module).name
		    client._send('{make,'+ erlang + '}.');
        },

        _send: function(message){
            if (this._ws)
                this._ws.send(message);
        },
       
        _onmessage: function(m) {
		    if (m.data) {
	            var str = m.data;
				eval(str);
           }
        },
        
        _onclose: function(m) {
          	this._ws=null;
        }   
    };
