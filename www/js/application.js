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
      console.log(data);
      switch(data.action) {
        case 'title':
          TypeSocial.setTitle(data.diff)
          break;
        default:
          console.log("I don't know this action");
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
    this.object.send({"doc_id":this.doc_id,"action":"title","diff":diff});
  }
}

var TypeSocial = {
  editor: null,
  title: null,
  title_last_rev:"",
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
    if (this.title_last_rev.length = 0 && this.title.val().length > 0) {
       this.title_last_rev = this.title.val();
       this.setTitle();
    }else{
      if (this.title_last_rev != this.title.val()) {
         
         this.setTitle();
      }
    }
  },
  setTitle: function(diff) {
    this.title.val(this.dmp.applyPatch(this.title.val(),diff));       
  },
  init: function(ext_config) {
    // Create the editor and set the config vars
    if (ext_config) {this.config = config}
    this.editor = $('#editor').ckeditor(this.onEditorReady,this.config); 

    this.title = $('#document_title');

    // Set up Diff Match Patch
    this.dmp.init();

    // Set up Socket.io
    this.socket.init(location.hostname,location.port);

    // Let's monitor title changes
    setInterval(this.checkTitle,999);


  }
}




$(document).ready(function(){

  TypeSocial.init();

});
