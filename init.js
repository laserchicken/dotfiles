// emacs as external editor
editor_shell_command = "emacsclient -c";

// view source in your editor
view_source_use_external_editor = true;

//Random start page///////////////////////////////////////////////////////////////////
//run thefreedictionary quiz whenever I start my browser
let (home = get_home_directory()) {
      home.appendRelativePath(".conkerorrc/thefreedictionary_quiz.html");
      homepage = home.path;
}

//////////////////////////////////////////////////////////////////////////////////////   
define_webjump("so", "http://stackoverflow.com/search?q=%s");
define_webjump("ling", "http://ling.pl/%s");
define_webjump("dict", "http://www.thefreedictionary.com/%s");
define_webjump("dict_source", "http://pl.thefreedictionary.com/%s");
define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&search=Search");
define_webjump("postgresql", "http://www.postgresql.org/search/?u=%2Fdocs%2F8.4%2F&q=%s");
define_webjump("trac-show-ticket", "https://subversion.ultimo.pl/trac/projects/ticket/%s");
define_webjump("hermes", "http://hermes.ultimo.pl/");

//Integrate conkeror with org-mode (capture),
//source http://emacs-fu.blogspot.com/2010/12/conkeror-web-browsing-emacs-way.html
//org-protocol stuff
function org_capture (url, title, selection, window) {
    var cmd_str =
        'emacsclient -c \"org-protocol:/capture:/w/'+url+'/'+title+'/'+selection+'\"';
    if (window != null) {
      window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-capture", "Clip url, title, and selection to capture via org-protocol",
          function (I) {
              org_capture(encodeURIComponent(I.buffer.display_uri_string),
                        encodeURIComponent(I.buffer.document.title),
                                encodeURIComponent(I.buffer.top_frame.getSelection()),
                        I.window);
          });
// capture with C-c c
define_key(content_buffer_normal_keymap, "C-c c", "org-capture");

////////////////////////////////////////////////////////////////////////////////////////////////
//Opens source code links (local) in emacs
//first hit "c" (copy link) and then run that function;
//used read_from_x_primary_selection function
//http://babbagefiles.blogspot.com/2011/01/conkeror-browsing-web-emacs-style.html
//emacsclient -n opens new buffer in existing emacs client instance
//TODO : what if client does not exists?
function emacs_open_source_url(window) {
    var source_url = read_from_x_primary_selection();

    var line_column_nr_url_regex = /#(.*):(.*)/;
    var line_column_nr = line_column_nr_url_regex.exec(source_url);
    var source_url_cleaned = source_url.replace("file://", "");
    var source_url_cleaned = source_url_cleaned.replace(/#.*/, "");

    if(line_column_nr) {
//column number must be always specified (may be 1) - looks like emacsclient call syntax thing
	
	var line_nr = line_column_nr[1];
	var column_nr = line_column_nr[2];
	var cmd_str =
            'emacsclient -n ' + '+' + line_nr + ':' + column_nr + ' ' + source_url_cleaned;

window.minibuffer.message('source: ' + cmd_str);		
    } else {
	var cmd_str =
            'emacsclient -n ' + source_url_cleaned;

window.minibuffer.message('source: ' + cmd_str);	
    }
	
    if (window != null) {
//	window.minibuffer.message('source: ' + source_url_cleaned);
//	window.minibuffer.message('line_column_nr: ' + line_column_nr);	
    }
    shell_command_blind(cmd_str);
}
interactive("emacs-open-source-url",
    "Opens source code at url in emacs",
    function (I) {
        emacs_open_source_url(
            I.window);	    
    });

// "C-c C-o" opens copied url
//(like link open in org document)
define_key(content_buffer_normal_keymap, "C-c C-o", "emacs-open-source-url");

////////////////////////////////////////////////////////////////////////////////////////////////
//Opens source code links (remote) in emacs
external_content_handlers.set("text/x-java", "emacsclient -n");

////////////////////////////////////////////////////////////////////////////////////////////////
//
url_remoting_fn = load_url_in_new_buffer;
