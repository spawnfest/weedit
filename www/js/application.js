var TypeSocial = {
  editor: null,
  socket:null,
  current_user:null,
  user_list:null,
  dmp:new diff_match_patch(),
  patch: function(diff_object) {

  },
  localtest:function(text)
  {
    
  },
  config: {
    toolbar:'Basic',
    resize_enabled:false,
    height:450,
    width:540
  },
  login: function(){},
  onEditorReady: function(){console.log("It's TypeSocialed!");},
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
