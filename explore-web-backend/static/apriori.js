/* * Some init hooks * */

$(document).ready(function(){
  initFilterConfig();
  initConstructorDialog();
});

/* * Socket, messages handling * */

wSocket.onclose = function(e) { showStatus('error', 'Connction to server closed!') };
wSocket.onmessage = function(msg) {
    console.log('received message: '.concat(msg.data));
    obj = JSON.parse(msg.data);
    
    switch(obj['type']) {
      case 'error': 
          processStatusMsg(obj, 'error');
          waitModal(false);
          break;
      case 'status':
          processStatusMsg(obj, 'ok');
          waitModal(false);
          break;
      case 'data-info':
          setDataInfo(obj);
          break;
      case 'rules':
          newAssocRules(obj['rules']);
          waitModal(false);
          break;
      case 'post-filter':
          handlePostFilterChange(obj);
          waitModal(false);
          break;
      case 'done':
          waitModal(false);
          break;
     }
    };

var sendMessageToServer = function(msgObj) {
  str = JSON.stringify(msgObj);
  console.log('send message: ' + str);
  wSocket.send(str); 
  waitModal(true); 
}
    
var processStatusMsg = function(obj, msgType){
  sTime = obj['showMillis']['Just'];
  if (sTime != undefined) showTime = parseInt(sTime);
  else showTime = undefined;
  
  showStatus(msgType, obj['message'], showTime);
};


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
  };

var waitModal = function(on) {
  $('#wait-modal').modal(on ? 'show' : 'hide');
};

/* * Data Info * */

var setDataInfo = function(inf) {
    $('#data-name').text(inf['name']);
    $('#data-attrs').text(inf['attrs']);
    $('#data-count').text(inf['count']);
};

/* * Assoc. Rules * */

var mkItems = function(itemset) {
  return itemset.reduce(
    function(acc, v, i){ item = '<item>' + v + '</item>'; 
                         return acc + (i==0 ? item : ', ' + item)
                       },
    "" 
  )
}

var mkAssocRule = function(rule) {
  left  = '<left>' + mkItems(rule['left']) + '</left>';
  right = '<right>' + mkItems(rule['right']) + '</right>';
  arrow = '<arrow><i class="glyphicon glyphicon-arrow-right"/></arrow>';
  support    = '<support>' + rule['support'] + '</support>';
  confidence = '<confidence>' + rule['confidence'] + '</confidence>';
  assocrule = '<assocrule class="row">' + left + arrow + right + support + confidence + '</assocrule>';
  return $(assocrule)
};

var _rulesSelector = function() { return $('#rules .rules-groups') };

var newAssocRules = function(rules) {
  _rulesSelector().children().remove();
  
  sup    = '<support>Support</support>'
  conf   = '<confidence>Confidence</confidence>'
  header = '<header class="container">' + conf + sup + '</header>';
  
  _rulesSelector().append($(header));
  
  for (i in rules) {
    group = rules[i].reduce( function(acc, rule){ return acc.add(mkAssocRule(rule)); }, $() );
    console.log('group = ' + group);
    _rulesSelector().append($('<group/>').append(group));
  }
};






var _random = function() { return (Math.random()).toString().substr(2) };







/* * Building Filters * */

var _filterPartSelected = null;

var filterContains = $('<cpart type="Contains">Contains <input type="text"/></cpart>');
var filterHas      = $('<cpart type="Has">Has Attribute <input type="text"/></cpart>');

var mkFilterNot = function() { return $('<cpart type="Not">Not </cpart>').append(mkFilterConstructor()) };


var mkFilter2Parts = function(name) { 
  return $('<cpart type="' + name + '"/>').append(mkFilterConstructor(), ' ' + name + ' ' , mkFilterConstructor()) 
}; 

var mkFilterConstructor = function() { 
  id = _random();
  return _filterPartSelectorSel().clone().attr('id', id).show()
          .append(
            $('<script/>')
              .append(prepareDropdownJS(id, 'create-contains', 'filterContains'))
              .append(prepareDropdownJS(id, 'create-has', 'filterHas'))
              .append(prepareDropdownJS(id, 'create-not', 'mkFilterNot()'))
              .append(prepareDropdownJS(id, 'create-and', 'mkFilter2Parts("And")'))
              .append(prepareDropdownJS(id, 'create-or',  'mkFilter2Parts("Or")'))
          )
};

var prepareDropdownJS = function(id, trigger, elem){
  return '$("#' + id + ' .' + trigger + '").click(function(e) { createFilterPartFromSelector(e.currentTarget, ' + elem + '.clone()) });'
};


var _filterPartSelectorSel = function() { return $('#filter-part-menu') };
var filterConfigSel        = function() { return $('.config.filter') };
var filtersListSel         = function() { return $('#filters-list') };


var createFilterPartFromSelector = function(t, part){ $(t).parents('.create-filter-part').replaceWith(part) };

var initFilterConfig = function(){
  $('.btn-success', filterConfigSel()).click(function (){ 
    _constructorServerCall = "post-filter";
    setConstructorContentsAndShow(mkFilterConstructor());
  });
};

/* * Add/Remove shown Post-Filters * */

var handlePostFilterChange = function(msg){
  id = msg['filter-id'];
  switch (msg['post-filter']){
    case 'new':
        filtersListSel().append(mkPostFilterDescription(id, msg['filter-str']));
        break;
    case 'remove':
        $(id).remove();
        break;
    case 'state':
        setPostFilterState(id, msg['state']);
        break;
  }
};

var mkPostFilterDescription = function(id, str){
  chbox = $('<input type="checkbox" checked>');
  descr = $('<description/>').append(str);
  pfilt = $('<post-filter id="' + id + '"/>');
  
  chboxSel = '$("#' + id +  ' :checkbox")';
  msg = '{ "elem-id": "post-filter", "filter-id": "' + id + '", "post-filter": "state", "state": state }' ;
  onSwitch = 'function(e, state) { e.stopPropagation(); sendMessageToServer(' + msg + '); }';
  script   = '.bootstrapSwitch({size: "mini", onSwitchChange: ' + onSwitch +'});';
  bToggle  = $('<script/>').append(chboxSel + script);
  return pfilt.append(chbox).append(descr).append(bToggle)
}

var setPostFilterState = function(id, state){
  box = $('#'+id+' :checkbox');
  switch (state){
    case true:
    case "true":
    case "True":
        box.bootstrapSwitch('state', true);
        break;
    case false:
    case "false":
    case "False":
        box.bootstrapSwitch('state', false);
        break;
  }
}




/* * Building Sorts * */





/* * Constructor Dialog * */

var _constructorServerCall = null;

var constructorDialogSel            = function() { return $('#constructor-dialog')  };
var constructorDialogContentsSel    = function() { return $('#constructor-dialog .contents')  };
var constructorDialogRuleSideSel    = function() { return $('#constructor-dialog .rule-side')  };

var setConstructorContentsAndShow = function(c){
  constructorDialogContentsSel().append(c);
  constructorDialogSel().modal('show');
}


var initConstructorDialog = function(){
  constructorDialogSel().on('hidden.bs.modal', function () { 
    constructorDialogContentsSel().children().remove(); 
    _constructorServerCall = null;
  });
  $('.modal-footer .btn-primary', constructorDialogSel()).click(handleConstructorSubmit);
}

var handleConstructorSubmit = function(event){
  
  side = $('.active :radio', constructorDialogRuleSideSel()).val()
  if(side == undefined) {
    event.stopPropagation();
    alert('Choose Rule Side');
  }
  else sendMessageToServer({ 'elem-id': _constructorServerCall, 
                             'post-filter': 'new', 
                             'rule-side': side, 
                             'builder': collectConstructed() 
                          })
}

var collectConstructed = function(){
  f = function(x){
    return $('> cpart', x).toArray().map(function(part) {
      var res = {};
      console.log('part = ' + part);
      tpe = part.attributes['type'].value;
      console.log('tpe = ' + tpe);
      switch (tpe){
        case 'Contains':
        case 'Has':
          res[tpe] = $('input', part).val();
          break;
        case 'Not':
          res[tpe] = f(part);
          break;
        case 'And':
        case 'Or':
          res[tpe] = f(part);
          break;
      }
      return res
    })
  }
  return f(constructorDialogContentsSel());
}

















