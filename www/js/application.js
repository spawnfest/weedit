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
  init: function(hostname,port) {
    this.object = new io.Socket(hostname, {port:port});

    this.object.on('connect', function(){
      console.log("We connected!!");
    });
  },
  doLogin: function(id,username) { 
    console.log("id and username = " + [id,username]); 
    //this.object.send('action':'login');
  },
  setTitle: function(diff) { 
    console.log(diff); 
    this.object.send({"action":"setTitle","diff":diff});
  }
}

var TypeSocial = {
  editor: null,
  title: null,
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
  receiveTitle: function(diff) {
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
    this.socket.init(location.hostname,'12000');

    this.socket.object.on('newtitle', function(data){
      console.log(data);
      TypeSocial.receiveTitle(data.diff);
    });
  }
}




$(document).ready(function(){

  TypeSocial.init();

});
