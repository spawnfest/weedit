var DiffMatchPatch = {
  object: null,
  init: function() {
    this.object = new diff_match_patch();
  },
  applyPatch: function(text1,text2,diff_object) {
    console.log("Patch");
    console.log("Text1 = " + text1);
    console.log("Text2 = " + text2);
    console.log("Diff = " + diff_object);

    patches = this.object.patch_make(text2, diff_object)

    console.log("Patches = " + patches);

    results = this.object.patch_apply(patches, text1);

    console.log("Results = " + results);
    return results[0];
  },
  getDiff: function(text1,text2) {
    diff = this.object.diff_main(text1, text2, true); 
    console.log("Diff = " + diff)
    if (diff.length > 2) {
      this.object.diff_cleanupSemantic(diff);
    }
    return diff;
  }
}

var TSocket = {
  object:null,
  init: function(hostname,port) {
    this.object = new io.Socket(hostname, {port:port});
  },
  doLogin: function(id,username) { 
    console.log("id and username = " + [id,username]); 
    //this.object.send('action':'login');
  },
  setTitle: function(title) { console.log(title); }
}


var TypeSocial = {
  editor: null,
  socket: null,
  current_user: null,
  user_list: null,
  dmp: DiffMatchPatch,
  localtest:function(text) {
    console.log("Local Text");
    console.log("Text = " + text)

    diff = this.dmp.getDiff(this.editor.val(), text); 
    this.editor.val(this.dmp.applyPatch(this.editor.val(),text,diff));
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
  init: function(ext_config) {
    // Set up Diff Match Patch
    this.dmp.init();

    // Set up Socket.io
    this.socket.init(location.hostname,'12000');
     
    // Create the editor and set the config vars
    if (ext_config) {this.config = config}
    this.editor = $('#editor').ckeditor(this.onEditorReady,this.config); 
    
  }
}




$(document).ready(function(){

  TypeSocial.init();

});
