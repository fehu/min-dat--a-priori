
wSocket.onclose = function(e) { showStatus('error', 'Connction to server closed!') };
wSocket.onmessage = function(msg) {
    obj = JSON.parse(msg);                    
    switch(obj['type']) {                     
      case 'error':                           
           showStatus('error', obj['error']); 
           break;                             
      case 'status':                          
          showStatus('ok', obj['status']);    
          break;                              
      case 'data-info':                       
          setDataInfo(obj['data-info']);      
          break;                              
      case 'rules':                           
          rulesUpdate(obj['rules']);          
          break;                              
     }                                        
    }



_closeBtn = '<a class=\"close\" data-dismiss=\"alert\" href=\"#\">&times;</a>'; 

var showStatus = function(type, msg){     
    var clazz = '';                       
    var extra = '';                       
    switch (type) {                       
        case 'error':                     
            clazz = 'alert-danger';        
            extra = '<h4 class=\"alert-heading\">Error!</h4>'; 
            break;                        
        case 'ok':                        
            clazz = 'alert-info'          
            break;                        
      };                                  
    str = '<div class=\"alert fade in '.concat(clazz).concat('\">')
            .concat(_closeBtn)
            .concat(extra)
            .concat(msg)
          .concat('</div>');
    $(str).prependTo('#statuses');        
  }
