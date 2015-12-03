
wSocket.onclose = function(e) { showStatus('error', 'Connction to server closed!') };
wSocket.onmessage = function(msg) {
    console.log('received message: '.concat(msg.data));
    obj = JSON.parse(msg.data);
    waitModal(false)

    switch(obj['type']) {
      case 'error': 
          processStatusMsg(obj, 'error')
          break;
      case 'status':
          processStatusMsg(obj, 'ok')
          break;
      case 'data-info':
          setDataInfo(obj);
          break;
      case 'rules':
          rulesUpdate(obj['message']);
          break;
     }
    }

var processStatusMsg = function(obj, msgType){
  sTime = obj['showMillis']['Just'];
  if (sTime != undefined) showTime = parseInt(sTime);
  else showTime = undefined;
  
  showStatus(msgType, obj['message'], showTime);
}


_closeBtn = '<a class=\"close\" data-dismiss=\"alert\" href=\"#\">&times;</a>';

var showStatus = function(type, msg, showTime){
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

    var status = $(str).prependTo('#statuses');

    if (showTime != undefined)
      window.setTimeout(function() { status.alert('close'); }, showTime);
  }

var setDataInfo = function(inf) {
    $('#data-name').text(inf['name']);
    $('#data-attrs').text(inf['attrs']);
    $('#data-count').text(inf['count']);
}

var waitModal = function(on) {
  $('#wait-modal').modal(on ? 'show' : 'hide');
}

