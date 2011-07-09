var TypeSocial = {
  editor: null,
  config: {
    toolbar:'Basic',
    resize_enabled:false,
    height:400,
    width:400
  },
  current_user:null,
  user_list:null,
  onReady: function(){console.log("It's TypeSocialed!");},
  init: function(ext_config) {
    if (ext_config) {this.config = config}
    this.editor = $('#editor').ckeditor(this.onReady,this.config); 
  }
}


$(document).ready(function(){

  TypeSocial.init();

});
