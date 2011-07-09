var TypeSocial = {
  editor: null,
  socket:null,
  current_user:null,
  user_list:null,
  dmp:new diff_match_patch(),
  socket: new io.Socket(location.hostname, {port:'12000'}),
  applyPatch: function(server_text,diff_object) {
    console.log("Patch");
    console.log("Server Text = " + server_text)
    console.log("Diff = " + diff_object)

    patches = this.dmp.patch_make(server_text, diff_object)

    console.log("Patches = " + patches)

    results = this.dmp.patch_apply(patches, this.editor.val());

    console.log("Results = " + results)
    this.editor.val(results[0]);
  },
  localtest:function(text) {
    console.log("Local Text");
    console.log("Text = " + text)

    diff = this.getDiff(this.editor.val(), text); 
    this.applyPatch(text,diff);
  },
  getDiff:function(text1,text2) {
    diff = this.dmp.diff_main(text1, text2, true); 
    console.log("Diff = " + diff)
    if (diff.length > 2) {
      this.dmp.diff_cleanupSemantic(diff);
    }
    return diff;
  },
  config: {
    toolbar:'Basic',
    resize_enabled:false,
    height:450,
    width:540
  },
  onEditorReady: function(element){
    console.log("It's TypeSocialed!");
    // Set event handlers to call on data change
    $(element).bind("setData.ckeditor", function(e){
      console.log("Here is the date" + $(this).val());
    });
  },
  init: function(ext_config) {
    // Set up Socket.io
     
    // Create the editor and set the config vars
    if (ext_config) {this.config = config}
    this.editor = $('#editor').ckeditor(this.onEditorReady,this.config); 
    
  }
}


$(document).ready(function(){

  TypeSocial.init();

});
