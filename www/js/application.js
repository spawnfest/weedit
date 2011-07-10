var DiffMatchPatch = {
  object: null,
  init: function() {
    this.object = new diff_match_patch();
    this.object.Match_Threshold = 0.25;
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
  timer:null,
  init: function(hostname,port) {
    this.object = new io.Socket(hostname, {port:port});
    this.object.connect();

    this.doc_id = location.pathname.split("/")[2];

    this.object.on('connect', function(){
      console.log("We connected!!");
      
    });


    this.object.on('message', function(data){
      switch(data.action) {
        case 'edit_title':
          TypeSocial.setTitle(data.diff)
          break;
        case 'edit_body':
          TypeSocial.setBody(data.diff)
          break;
        case 'set_hash_tags':
          $('#addterm').spinner('remove');
          AddHashTerm.loadlist(data.tags);
          break;
        case 'tweet':
          AddTweet.load(data.tweet,'twitter');
          break;
        case 'set_users':
          console.log(data.users);
          RefreshClientList.load(data.users);
          break;
        default:
          console.log("I don't know this action" + data);
      }
    });
  },
  doHello: function(){
    this.object.send({"doc_id":this.doc_id,"action":"hello"});
    clearInterval(this.timer);
  },
  doLogin: function(screen_name, image_url) { 
    console.log("id and username = " + [screen_name,image_url]); 
    this.object.send({'action':'login',"doc_id":this.doc_id,"username":screen_name, "imageurl":image_url});
  },
  doSetTitle: function(diff) { 
    console.log(diff); 
    this.object.send({"doc_id":this.doc_id,"action":"edit_title","diff":$.base64.encode(JSON.stringify(diff))});
  },
  doSetDoc: function(diff) { 
    console.log(diff); 
    if (this.object)
      this.object.send({"doc_id":this.doc_id,"action":"edit_body","diff":$.base64.encode(JSON.stringify(diff))});
  },
  doSetHashTags: function(terms) { 
    console.log("sending terms ");
    console.log(terms);
    if (this.object)
      this.object.send({"doc_id":this.doc_id,"action":"set_hash_tags","tags":terms});
  },
  startInterval: function(){
    var instance = this;
    this.timer = setInterval(function() {instance.doHello();},500);
  }
}

var TypeSocial = {
  editor: null,
  editor_last_rev:"",
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
  checkDiffChanges: function () {
	  
    //console.log("Checking title...");

    if (this.title_last_rev != this.title.val()) {
       console.log("title changed...");
       diff = this.dmp.getDiff(this.title_last_rev,this.title.val());
       this.title_last_rev = this.title.val();
       this.socket.doSetTitle(diff);
    }

    //console.log("Checking doc...");
    var content = "";
    if (this.editor.tinymce()) { 
      content = this.editor.tinymce().getContent();
    } else { content = this.editor.val(); }

    if (this.editor_last_rev != content) {
      console.log("doc changed...");
      console.log(this.editor_last_rev);
      console.log(content);

      diff = this.dmp.getDiff(this.editor_last_rev,content);
      this.editor_last_rev = content;
      this.socket.doSetDoc(diff);
    }

  },
  setTitle: function(diff) {
    this.title.val(this.dmp.applyPatch(this.title.val(),diff));       
    this.title_last_rev = this.title.val();
  TSocket},
  setBody: function(diff) {
    if (this.editor.tinymce()) { 
      this.editor.tinymce().setContent(this.dmp.applyPatch(this.editor.tinymce().getContent(),diff));       
      this.editor_last_rev = this.editor.tinymce().getContent();
    } else { 
      this.editor.val(this.dmp.applyPatch(this.editor.val(),diff));       
      this.editor_last_rev = this.editor.val();
    }
  },
  startInterval: function(interval){
    var instance = this;
    this.timer = setInterval(function() {instance.checkDiffChanges();},interval);
  },
  stopInterval: function() {
    clearInterval(this.timer);
  },

  init: function(ext_config) {

    $('#addterm').spinner({ position: 'center', hide: true });

    // Create the editor and set the config vars
    if (ext_config) {this.config = config}

    //this.editor = $('#editor').ckeditor(this.onEditorReady,this.config); 
    $('#editor').tinymce({theme : "advanced", theme_advanced_buttons2 : "",
              theme_advanced_toolbar_location : "top",
              theme_advanced_blockformats:"",
              theme_advanced_toolbar_align : "left",height : "480"});

    this.editor = $('#editor');

    // Setting the title object and the value for the last revision
    // in case it comes from the server
    this.title = $('#document_title');
    this.title_last_rev = this.title.val();

    if (this.editor.tinymce()) {
      this.editor_last_rev = this.editor.tinymce().getContent();
    }else{
      this.editor_last_rev = this.editor.val();
    }

    // Set up Diff Match Patch
    this.dmp.init();

    this.dmp.Match_Threshold = 0.2;

    this.dmp.Patch_DeleteThreshold = 0.2;

    // Set up Socket.io
    if (location.hostname != '') {
      this.socket.startInterval();
    	this.socket.init(location.hostname,location.port);
    }

    // Let's monitor title changes
    this.startInterval(500);


  }
}

var RefreshClientList = {
	load: function(users) {
		console.log(users);
	  	var userlist	= '';
		$.each(users, function() {
			var user  = this.username;

			if ($("#"+user).length == 0) {
				if (userlist == '') {
					userlist	= user;
				} else {
					userlist += ',' + user;
				}
			}
		});

        var imageurl  = '';

        $.ajax({
        	url: "http://api.twitter.com/1/users/lookup.json?screen_name=" + userlist,
        	dataType: "jsonp",
        	success:function(data,text,xhqr) {
        		var username	= '';
        		var imageurl	= '';
        		$.each(data, function(key, val) {
        			if (key == 'screen_name') {
        				username	= val;
        			} else if (key == 'profile_image_url') {
        				imageurl  = val;
        			}
        		});
        		
        		if(username != '' && imageurl != '') {
        			$('#userlist').append('<div class="twitteritem" id="' + username + '"><img id="twitter_avatar" src="' + imageurl + '"><span id="handle">' + username + '</span></div>')
        		}
        	},	
        	error:function(jqXHR, textStatus, errorThrown) {
        		console.log("Error processing twitter user list " + textStatus);
        	}
        });
	}
}

var AddTweet = {	
	load: function(tweet,type) {
    var text  = '';
    var img   = '';

    if (type == 'twitter') {
  	  text	= '';
      img   = tweet.user.profile_image_url;

      console.log(tweet);
		  if (tweet.text.length >= 88) {
        url   = '<a href="http://twitter.com/#!/' + tweet.user.screen_name + '" target="_blank">...</a>';
  		  text	= tweet.text.substring(0,88) + ' ' + url;
	  	} else {
		 		text	= tweet.text;				
		  }
    } else {
      text  = tweet;
      img   = "images/logo.png";
    }
						
		if($("#tweets > div").size() == 5) {
			$('<div><div class="tweet" id="' + this + '"><img id="twitter_avatar" align="left" src="'+ img + '">' + text + '</div></div><hr id="'+this+'">').hide().appendTo('#tweets').delay(500).fadeIn(1000);
		} else {
			$('<div><div class="tweet" id="' + this + '"><img id="twitter_avatar" align="left" src="' + img + '">' + text + '</div></div><hr id="'+this+'">').hide().appendTo('#tweets').delay(500).fadeIn(1000);
		}
	}
}

var AddHashTerm = {
	add: function(term) {

			
		if($("#searchterms").size() == 10) {
			$('#addterm').remove();
		}
		
		var terms = new Array();
		$("#searchterms > div").each(function(index, domEle) {
			console.log($(this).text());
			terms.push($(this).text().replace(/^#/,''));
		});
		
    terms.push(term);

		TSocket.doSetHashTags(terms);
		
		
	},
	loadlist: function(jsonlist) {		
    console.log(jsonlist);
    var new_search  = '';
    var search_arr  = new Array();

    $('#searchterms').children().remove("div");
    $.each(jsonlist, function(i,val){
  	  sanitizedterm		= "#" + val;

			$('<div id="'+ sanitizedterm + '" class="searchterm">' + sanitizedterm + ' <a id="'+sanitizedterm+'" href="#" onclick="javascript:$(this).parent().remove();"><img id="delete" src="images/delete_icon.png"></a></div>').hide().appendTo('#searchterms').delay(500).fadeIn(1000);		

      if (new_search == '') {
        new_search  = "New search term added.  Now searching on " + sanitizedterm;
      } else {
        new_search  += ', ' + sanitizedterm;
      }
    });
	
	  search_arr.push(new_search);

	  AddTweet.load(search_arr,'terms');
	},
	deleteitem: function(div_text) {
		var terms = new Array();
		$("#searchterms > div").each(function(index, domEle) {		
			if ($(this).text() != div_text) {
				terms.push($(this).text().replace(/^#/,''));
			}
		});
		
		TSocket.doSetHashTags(terms);		
	}
}

var LoginBox = {
	init: function() {
	
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
	     
	    //if close button is clicked
	    $('.window .close').click(function (e) {
	        //Cancel the link behavior
	        e.preventDefault();
	        $('#mask, .window').hide();
	    });	    	   
	   	
	    twttr.anywhere(function (T) {	    	
	    	document.getElementById("follow-placeholder").onclick = function () {
	    		T.signIn();
	    	};
		});
	    //if mask is clicked
	    //$('#mask').click(function () {
	    //    $(this).hide();
	    //    $('.window').hide();
	    //});	    
	}
}

var LoadSearchTerm = {
	open: function() {
	 	
	    //Cancel the link behavior
	    //e.preventDefault();
	    //Get the A tag
		var id = $('#newsearchterm');
	 
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
     
	    //if close button is clicked
	    $('.window .addnewterm').click(function (e) {
	        //Cancel the link behavior
	        e.preventDefault();	       	   
          LoadSearchTerm.SubmitTerm(id);
	    });


      $('#searchterminput').keyup(function(e) {
        if (e.keyCode == 13) {
          LoadSearchTerm.SubmitTerm(id);
        }
      });
	     
	    //if mask is clicked
	    $('#mask').click(function () {
	        $(this).hide();
	        $('.window').hide();
	        $('#searchterminput').val('');
	    });
	},
  SubmitTerm: function(id)
  {
    console.log("IM HERE");
    console.log($('#searchterminput').val());
    if ($('#searchterminput').val() != ''){
        AddHashTerm.add($('#searchterminput').val().replace(/^#/,''));
        $('#mask, .window').hide();
        $('#searchterminput').val('');        
        $('#searchterminput').unbind('keyup');
        $('.window .addnewterm').unbind('click');
    }else{
      $(id).effect( 'shake', {}, 100);
    }

  }
}

var LoadTweetBox = {
	init: function() {
		twttr.anywhere("1",function (T) {
		
			T("#tbox").tweetBox({
				label: "Thoughts?",
		  		height: 50,
		  		width: 190,
		  		defaultContent: "#typesocial " + location + " "
			});
		});
	}
}


$(document).ready(function(){
  TypeSocial.init();
  LoadTweetBox.init();

  $('#addterm').click(function() {
	  LoadSearchTerm.open();
  });
  
  twttr.anywhere(function(twitter) {  
	    if(twitter.isConnected()){  
        try {
        	console.log(twitter);
	        var twitteruser		= twitter.currentUser.data('screen_name');
	        var twitterimgurl	= twitter.currentUser.data('profile_image_url');
	        console.log("User " + twitteruser + " is logged in");
  	        TSocket.doLogin(twitteruser,twitterimgurl);
        } catch(error) {
            console.log(error);
        }
	    } else {  
	    	LoginBox.init();
	    }  
	});


});
