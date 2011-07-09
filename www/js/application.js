var DiffMatchPatch = {
  object: null,
  init: function() {
    this.object = new diff_match_patch();
  },
  applyPatch: function(text1,diff_object) {
    console.log("Patch");
    console.log("Text1 = " + text1);

    patches = this.object.patch_make(text1,diff_object)

    console.log("Patches = " + patches);

    results = this.object.patch_apply(patches, text1);

    console.log("Results = " + results);
    return results[0];
  },
  getDiff: function(text1,text2) {
    diff = this.object.diff_main(text1, text2, true); 
    if (diff.length > 2) {
      this.object.diff_cleanupSemantic(diff);
    }

    console.log("Diff = " + diff);
    return diff;
  }
}

var TSocket = {
  object:null,
  doc_id:null,
  init: function(hostname,port) {
    this.object = new io.Socket(hostname, {port:port});
    this.object.connect();

    this.doc_id = location.pathname.split("/")[2];

    this.object.on('connect', function(){
      console.log("We connected!!");
      TSocket.doHello();
    });


    this.object.on('message', function(data){
      switch(data.action) {
        case 'edit_title':
          TypeSocial.setTitle(data.diff)
          break;
        default:
          console.log("I don't know this action" + data);
      }
    });

  },
  doHello: function(){
    this.object.send({"doc_id":this.doc_id,"action":"hello"});
  },
  doLogin: function(id,username) { 
    console.log("id and username = " + [id,username]); 
    //this.object.send('action':'login');
  },
  doSetTitle: function(diff) { 
    console.log(diff); 
    this.object.send({"doc_id":this.doc_id,"action":"edit_title","diff":diff});
  }
}

var TypeSocial = {
  editor: null,
  title: null,
  title_last_rev:"",
  timer:null,
  socket: null,
  current_user: null,
  user_list: null,
  dmp: DiffMatchPatch,
  TestTitleChange: function(text) {
    console.log("Local Text");
    console.log("Text = " + text);
    diff = this.dmp.getDiff(this.title.val(),text);
    this.title.val(this.dmp.applyPatch(this.title.val(),diff));
  },
  socket: TSocket,
  config: {
    toolbar:'Basic',
    resize_enabled:false,
    height:450,
    width:540
  },
  onEditorReady: function(element){
    console.log("It's TypeSocialed!");
    // This only works when called from javascript
    // Set event handlers to call on data change
    $(element).bind("setData.ckeditor", function(event) {
      console.log("Here is the data" + event);
    });
  },
  checkTitle: function () {
    console.log("Checking title...");

    if (this.title_last_rev != this.title.val()) {
       console.log(this);
       diff = this.dmp.getDiff(this.title.val(),this.title_last_rev);
       this.title_last_rev = this.title.val();
       this.socket.doSetTitle(diff);
    }
  },
  setTitle: function(diff) {
    this.title.val(this.dmp.applyPatch(this.title.val(),diff));       
    this.title_last_rev = this.title.val();
  },
  startInterval: function(interval){
    var instance = this;
    this.timer = setInterval(function() {instance.checkTitle();},interval);
  },
  stopInterval: function() {
    clearInterval(this.timer);
  },
  init: function(ext_config) {

    // Create the editor and set the config vars
    if (ext_config) {this.config = config}
    this.editor = $('#editor').ckeditor(this.onEditorReady,this.config); 

    this.title = $('#document_title');

    // Set up Diff Match Patch
    this.dmp.init();
    // Set up Socket.io
    if (location.hostname != '') {
    	this.socket.init(location.hostname,location.port);
    }

    // Let's monitor title changes
    this.startInterval(1000);


  }
}

var RefreshClientList = {
	//TODO: Parse JSON
	load: function() {
		var users=["Chad","Fernando","Manuel","Matt", "Matt","Chad"];
	
		jQuery.each(users, function() {
			if ($("#" + this).length == 0){			
				$('#userlist').append('<div class="twitteritem" id="' + this + '"><img id="twitter_avatar" src="images/twitter_logo.png"><span id="handle">' + this + '</span></div>')
			}
		});
	}
}

var RefreshTweetList = {
	//TODO: Parse JSON
	load: function() {
		var tweets=["more test text","This is some text","this is some more test text","this text is awesome","more and more text", "this is the absolute maximum size of a tweet.  this should not overflow and the tweet div maximum should be set to this height, not more ok!"];
	
		jQuery.each(tweets, function() {
			var text	= '';
			if (this.length >= 100) {
				//TODO: Fix url
				text	= this.substring(0,100) + ' <a href="#">...</a>';
			} else {
				text	= this;				
			}
						
			if($("#tweets > div").size() == 5) {
				$('#tweets').find('div').first().fadeOut(500).remove();
				$('<div><div class="tweet" id="' + this + '"><img id="twitter_avatar" align="left" src="images/twitter_logo.png">' + text + '</div></div><hr>').hide().appendTo('#tweets').delay(500).fadeIn(1000);
			} else {
				$('<div><div class="tweet" id="' + this + '"><img id="twitter_avatar" align="left" src="images/twitter_logo.png">' + text + '</div></div><hr>').hide().appendTo('#tweets').delay(500).fadeIn(1000);
			}
		});
	}
}

var LoginBox = {
	init: function() {
	 	var	getridofthiswhenwehavesession = '';
		
		if (getridofthiswhenwehavesession == '') {
	        //Cancel the link behavior
	        //e.preventDefault();
	        //Get the A tag
	        //var id = $(this).attr('href');
			var id = $('#dialog');
	     
	        //Get the screen height and width
	        var maskHeight = $(document).height();
	        var maskWidth = $(window).width();
	     
	        //Set height and width to mask to fill up the whole screen
	        $('#mask').css({'width':maskWidth,'height':maskHeight});
	         
	        //transition effect     
	        $('#mask').fadeIn(1000);    
	        $('#mask').fadeTo("slow",0.8);  
	     
	        //Get the window height and width
	        var winH = $(window).height();
	        var winW = $(window).width();
			
			//Set the popup window to center
			$(id).css('top',  winH/2-$(id).height()/2);
			$(id).css('left', winW/2-$(id).width()/2);
	              
	        //transition effect
	        $(id).fadeIn(2000); 
	     }
	    
	     
	    //if close button is clicked
	    $('.window .close').click(function (e) {
	        //Cancel the link behavior
	        e.preventDefault();
	        $('#mask, .window').hide();
	    });     
	     
	    //if mask is clicked
	    $('#mask').click(function () {
	        $(this).hide();
	        $('.window').hide();
	    });
	}
}

var LoadTweetBox = {
	init: function() {
		twttr.anywhere(function (T) {
		
			T("#tbox").tweetBox({
				label: "Thoughts?",
		  		height: 50,
		  		width: 190,
		  		defaultContent: "<Type Socially!>"
			});
		});
	}
}
$(document).ready(function(){

  TypeSocial.init();
  RefreshClientList.load();
  RefreshTweetList.load();
  LoadTweetBox.init();
  LoginBox.init();
});
