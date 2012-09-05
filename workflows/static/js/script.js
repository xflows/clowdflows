// The Widget class
function Widget(name,icon,inputs,outputs,preferences,action) {
    this.name = name;  // string
    this.icon = icon; // string
	this.inputs = inputs; // array of InputOutput
	this.outputs = outputs; // array of InputOutput
	this.outputdata = new Array();
	this.preferences = preferences; //array of Preference
	this.action = action;
	this.canvas = 0;
	this.counter = 0;
	this.parent = 0;
	this.type = 0;
	this.deleted = 0; // 0 - widget not deleted, 1 - current instance of the widget deleted from the workflow
	this.state=0; // 0 - current instance of the widget has not yet been run for this input, 1 - current instance of the widget is currently being executed, 2 - appropriate data based on the inputs is available on the widget's output
	this.interval=0;
	this.error=0;
}
function Canvas(name,activeWidgetId) {
	this.activeWidgetId = activeWidgetId;
}
function InputOutput(name,type,required) {
	this.name = name; // string
	this.type = type; // string, used for checking if data matches
	this.required = required; // boolean
	this.deleted = 0;
	this.widgetLink = 0;
	this.title = "";
}
function InputOutput(name,type,required,widgetLink,counter) {
	this.name = name; // string
	this.type = type; // string, used for checking if data matches
	this.required = required; // boolean
	this.deleted = 0;
	this.widgetLink = widgetLink;
	this.counter = counter;
	this.title = "";
}
function Connection(output,input,outputWidget,inputWidget) {
	this.output = output; // (IMPORTANT!!) A string description of the OUTPUT side of the connection. If this is the first output on the widget with id = widget2 then this.output = "widget-2-0"
	this.input = input; // (IMPORTANT!!) A string description of the INPUT side of the connection. If this is the first input on the widget with id = widget2 then this.input = "widget-2-0"
	this.inputWidget = inputWidget; // (IMPORTANT!!) An integer describing the ID of the widget on the input side of the connection in question
	this.outputWidget = outputWidget; // (IMPORTANT!!) An integer describing the ID of the widget on the output side of the connection in question
	this.deleted = 0;	
}
function Preference(label,type,name,defaultvalue) {
	this.name = name; // string
	this.type = type; // string
	this.label = label; // string
	this.defaultvalue = defaultvalue; //string
	this.deleted = 0;
	this.values = "";
}
var subprocessCounter = 1;
var subInputCounters = new Array();
var subOutputCounters = new Array();
//
var canvasCounter = 1;
var activeCanvasId = 0;
var activeCanvas = "";
var importId=0;
var loadedUserWidgets = new Array();
var loadedUserWidgetsCounter = 0;
// The array activeWidgets conatins an instance of the Widget class for each instance of a widget added on the canvas. Deleted widgets are also kept in the array.
var activeWidgets = new Array();
var activeWidgetsCounter = 1;
// Each element of the preconditions array is an array of widget IDs that need to be executed in full before the current widget may be executed (i.e. preconditions[3] = array(1,2) means that #widget1 and #widget2 must be executed in full before #widget3
var preconditions = new Array();
// The array connections contains instances of the class Connection. Deleted connections are also kept in the array.
var connections = new Array();
var connectionsCounter = 0;
// The array loadedWidgets is the current active roster of widgets. Each one of these widgets may be transfered onto the canvas. Once this is done the clone of this object is added to the activeWidgets array.
var loadedWidgets = new Array();
var loadedWidgetsCounter = 0;
// This variable is important because it is used to inject the id of the connection into the library that draws lines. It enables us to distinct between connections so that we may select and delete connections.
// During the drawing the variable's value represents the ID of the connection being drawn
var drawingConnection = -1;
// These variables contain IDs of selected elements. Only one of each may be selected at the same time. Both a widget and a connection may not be selected at the same time.
var selectedWidget = "";
var selectedInput = "";
var selectedOutput = "";
var selectedConnection = -1;
// Whether or not direct lines are to be used while drawing connections, this variable is set in the preferences dialog
var drawStraightLines = false;
// Object used for drawing lines
var jg;
// Functions used to change the text in the error box in the header of the application. These functions change the icon and the background color of the box.
function reportError(errorMessage) {
	$("#status").find(".infotext").html(errorMessage);
	$("#status").find(".ui-icon").addClass("ui-icon-alert");
	$("#status").find(".ui-icon").removeClass("ui-icon-info");
	$("#status").find(".ui-icon").removeClass("ui-icon-circle-check");
	$("#status").removeClass("ui-state-highlight");
	$("#status").addClass("ui-state-error");
}
function reportOk(statusMessage) {
	$("#status").find(".infotext").html(statusMessage);
	$("#status").find(".ui-icon").removeClass("ui-icon-alert");
	$("#status").find(".ui-icon").removeClass("ui-icon-info");
	$("#status").find(".ui-icon").addClass("ui-icon-circle-check");
	$("#status").addClass("ui-state-highlight");
	$("#status").removeClass("ui-state-error");
}
function reportStatus(statusMessage) {
	$("#status").find(".infotext").html(statusMessage);
	$("#status").find(".ui-icon").removeClass("ui-icon-alert");
	$("#status").find(".ui-icon").addClass("ui-icon-info");
	$("#status").find(".ui-icon").removeClass("ui-icon-circle-check");
	$("#status").addClass("ui-state-highlight");
	$("#status").removeClass("ui-state-error");
}
/* resizeWidgets() selects all currently active widgets, counts their inputs and outputs and resizes them accordingly, 
so that graphically they appear longer when there are larger number of inputs/outputs used. Each excess input/output adds 29pixels
to the height of the widget. The icon representing the widget is then vertically aligned to the middle of the widget */
function resizeWidgets() {
	$("div.canvas div.widget").each(function () {
		image = $(this).find("img").not(".loadingimage");	
		inputs = $(this).children("div.inputs").children("div").not(".WireIt-Wire-scissors").size();
		outputs = $(this).children("div.outputs").children("div").not(".WireIt-Wire-scissors").size();
		max = inputs;
		if (outputs>inputs) {
			max=outputs;
		}
		if (image.height()>max*29) {
			max = image.height()/29;
		}
		$(this).find(".widgetcenter").height((max*29)+'px');
		
		thisWidget = $(this);
		
		
		image.load(function() {
		
			if ($(this).height()>max*29) {
				max = image.height()/29;
			}
			thisWidget.find(".widgetcenter").height((max*29)+'px');
		
			$(this).css('top',((max*29/2)-(image.height()/2))+'px');
			
			thisWidget.css('width','auto');
			
			thisWidget.css('width',thisWidget.width());
	
		});
	
	});
}
/* updateConnectionListeners() should be called when a new connection has been drawn on the canvas.
All events concerning connections are described in this function.
It enables clicking on connections and changing their colors when
hovering over them with the mouse for easier understanding of the workflow. */
function updateConnectionListeners() {
	/*$(".connection").click(function() {
		$(".connection").removeClass("selectedline");		
		connectionid = $(this).attr('rel');
		selectedConnection=connectionid;
		$(".connection"+connectionid).addClass("selectedline");
		$(".widgetcenter").removeClass("ui-state-highlight");
		selectedWidget="";	
	});
	$(".connection").mouseenter(function() {
		connectionid = $(this).attr('rel');
		$(".connection"+connectionid).addClass("hoveredline");
	});*/
	$(".drawingcanvases").unbind('mouseenter');
	$(".drawingcanvases").mouseenter(function() {
		connectionid = $(this).attr('rel');
		currentTop=$(this).css('top');
		currentLeft=$(this).css('left');
		drawConnection(connectionid,'#ff0000','#ffaaaa');
		$(this).css('top',currentTop);
		$(this).css('left',currentLeft);
	});
	
	$(".drawingcanvases").unbind('mouseleave');
	$(".drawingcanvases").mouseleave(function() {
		connectionid = $(this).attr('rel');	
		currentTop=$(this).css('top');
		currentLeft=$(this).css('left');	
		drawConnection(connectionid);		
		$(this).css('top',currentTop);
		$(this).css('left',currentLeft);		
	});
	
	$(".drawingcanvases").unbind('click');
	$(".drawingcanvases").click(function() {
		connectionid = $(this).attr('rel');
		selectedConnection=connectionid;
		currentLeft=$(this).css('left');
		currentTop=$(this).css('top');		
		drawConnection(connectionid);		
		$(this).css('top',currentTop);
		$(this).css('left',currentLeft);
		$(".widgetcenter").removeClass("ui-state-highlight");
		selectedWidget="";			
		
		$(".drawingcanvases").each(function() {
			connectionid = $(this).attr('rel');	
			currentLeft = $(this).css('left');
			currentTop=$(this).css('top');		
			drawConnection(connectionid);		
			$(this).css('top',currentTop);
			$(this).css('left',currentLeft);			
		});
		
		
	});
	/*
	$(".connection").mouseleave(function() {
		connectionid = $(this).attr('rel');
		$(".connection"+connectionid).removeClass("hoveredline");		
	});*/	
	
}
function isSubOf(parent,child) {
	if (activeWidgets[child].canvas==0&&parent!=0) {
		return false;
	
	}
	
	if (activeWidgets[child].canvas==parent||child==parent) {
		return true;
	} 
	if ((activeWidgets[child].parent)!=0) {
		return isSubOf(parent,activeWidgets[child].parent);
	} else {
		return false;
	}
}
/* works in a similar way to updateConnectionListeners(), it should be called when a new widget has been drawn on the canvas.
Each widget may be dragged along the canvas. While the users is dragging the widget the lines are redrawn in real time (unless direct lines are used) 
Clicking on a widget selects it. Clicking on an input of a widget selects the widget and the input. Clicking on an output of the widget selects the widget and the output.
When clicking on an input a check is made to determine whether an output is already selected. If yes, a connection is attempted.
In the same way when clicking on an output a check is made to determine whether an input is already selected. If selected, a connection is attempted.
*/
function updateWidgetListeners() {
		$("#dialogs div.widgetdialog").dialog({
		autoOpen: false,
		modal: false,
		resizable: true,
		buttons: {
				"Close": function() { 
					$(this).dialog("close"); 
				} 
			}
		});	
		
		$(".widgetnameinput").change(function() {
		
			w = $(this).parent().parent().attr('rel');
			
			activeWidgets[w].name = $(this).val();
			
			$("#widgetcaption"+w).html($(this).val());
			
			$("span[rel=#canvas"+w+"]").html($(this).val());
			
		
		});
		$(".canvas div.widget").each(function() {
		
			var thisWidgetId = $(this).attr('rel');
			//alert(thisWidgetId);
		
			if ($(this).data('contextMenu')!=true) {
				$(this).contextMenu({
					menu: 'widgetMenu'
				},
					function(action, el, pos) {
					
					if (action=='delete') {
						selectedWidget = $(el).attr('id');
						$(".widgetcenter").removeClass("ui-state-highlight");
						$(el).find(".widgetcenter").addClass("ui-state-highlight");
						selectedConnection=-1;
						$(".connection").removeClass("selectedline");	
						deleteSelected();
					}
					
					if (action=='run') {		
						calculatePreconditions();
						w = $(el).attr('rel');
							if (activeWidgets[w].deleted==0&&activeWidgets[w].state!=1) {
								
								satisfied = true;
								if (preconditions[w].length==0) {
									satisfied=true;
								} else {
								
									for (prec in preconditions[w]) {
										if (activeWidgets[preconditions[w][prec]].state!=2) {
											satisfied=false;
										}
									}
									
								}
								if (satisfied) {
									if (checkRequirementsOfWidget(w)) {
										changeState(w,0);
										runWidget(w,false);
									} else {
										reportError("Required inputs are missing.");
									}
								} else {
									reportError("Preconditions for running this widget have not been met");
								}
							}
					}
					
					if (action=='properties') {
						w = $(el).attr('rel');
							$("#widgetpreferences-"+w).dialog('open');
					}			
					
					if (action=='results') {
						w = $(el).attr('rel');
							$("#widgetresults-"+w).dialog('open');
					}
					
					if (action=='rename') {
						w = $(el).attr('rel');
							$("#widgetrename-"+w).dialog('open');
					}					
					
					if (action=='savewidget') {
					
						topMostCanvasId = $(el).attr('rel');
						xmlstring = '<?xml version="1.0" encoding="utf-8"?>\n';
						xmlstring += '<workflow>\n';
						xmlstring += '<widgets>\n';
						for (w in activeWidgets) {
							//alert(topMostCanvasId+" "+w);
							if (isSubOf(topMostCanvasId,w)||w==topMostCanvasId) {
								//alert("true");
								top = $("#widget"+w).css('top');
								left = $("#widget"+w).css('left');
								xmlstring += '  <widget>\n    <top>'+top+'</top>\n    <left>'+left+'</left>\n    <id>'+w+'</id>\n    <name>'+activeWidgets[w].name+'</name>\n    <icon>'+activeWidgets[w].icon+'</icon>\n';
								xmlstring += '    <preferences>\n';
								for (p in activeWidgets[w].preferences) {
									xmlstring += "    <pref type='"+activeWidgets[w].preferences[p].type+"' name='"+activeWidgets[w].preferences[p].name+"' defaultvalue='"+($("#pref-"+w+"-"+activeWidgets[w].preferences[p].name).val())+"'>"+activeWidgets[w].preferences[p].label+"</pref>\n";
								}
								xmlstring += '    </preferences>\n';
								xmlstring += '    <inputs>\n';
								for (i in activeWidgets[w].inputs) {
									xmlstring += "        <input type='"+activeWidgets[w].inputs[i].type+"' required='"+activeWidgets[w].inputs[i].required+"' widgetLink='"+activeWidgets[w].inputs[i].widgetLink+"'>"+activeWidgets[w].inputs[i].name+"</input>\n";
								}
								xmlstring += '    </inputs>\n';
								xmlstring += '    <outputs>\n';
								for (o in activeWidgets[w].outputs) {
									xmlstring += "        <output type='"+activeWidgets[w].outputs[o].type+"' widgetLink='"+activeWidgets[w].outputs[o].widgetLink+"'>"+activeWidgets[w].outputs[o].name+"</output>\n";
								}
								xmlstring += '    </outputs>\n';
								xmlstring += '    <action>'+activeWidgets[w].action+'</action>\n    <canvas>'+activeWidgets[w].canvas+'</canvas>\n    <counter>'+activeWidgets[w].counter+'</counter>\n    <type>'+activeWidgets[w].type+'</type>\n    <deleted>'+activeWidgets[w].deleted+'</deleted>\n  </widget>\n'
							}
						}
						xmlstring += '</widgets>\n';
						xmlstring += '<connections>\n';
						for (c in connections) {
							if (connections[c].deleted==0) {
							
								if (isSubOf(topMostCanvasId,connections[c].inputWidget)&&connections[c].inputWidget!=topMostCanvasId&&connections[c].outputWidget!=topMostCanvasId) {
							
								xmlstring += "  <connection output='"+connections[c].output+"' input = '"+connections[c].input+"' /> \n";
								
								}
							}
						}
						xmlstring += '</connections>\n';
						xmlstring += '</workflow>\n';
						$("#subprocesssave").val(xmlstring);
						document.forms.savesub.submit();
					}
						
				});
				$(this).data('contextMenu',true);
				$(this).bind("dblclick", function(){
					if (activeWidgets[thisWidgetId].type==0) {
						$("#widgetpreferences-"+$(this).attr('rel')).dialog('open');
					} else {
						$("#tabs").tabs("select","#canvas"+thisWidgetId);
						activeCanvasId = thisWidgetId;
						activeCanvas = $(".canvas"+activeCanvasId);
						resizeCanvas();
						
					}
				});
			}
		
		
		});
		$(".canvas div.widget").draggable({
			handle: "div.widgetcenter",
			drag: function() {
				// this function exectues every time the mouse moves and the user is holding down the left mouse button
				for (c in connections) {
				
					if (connections[c].inputWidget==$(this).attr('rel')&&(connections[c].deleted==0)) {
						$(".connection"+c).remove();
						if (!drawStraightLines) {
						drawConnection(c);
						}
					}
					
					if (connections[c].outputWidget==$(this).attr('rel')&&(connections[c].deleted==0)) {
					
						$(".connection"+c).remove();
						if (!drawStraightLines) {
						drawConnection(c);
						}
					}
					
				}
				
			},
			
			stop: function() {
				// this function exectues when the user stops dragging the widget
					if (($(this).css('left')).charAt(0)=='-') {
						$(this).css('left','0px');
					}
					if (($(this).css('top')).charAt(0)=='-') {
						$(this).css('top','0px');
					}
					// if the dragged widget has been dragged to the left side of the canvas or over the top of the canvas it is put back onto the canvas so that it is not lost to the user					
			
					for (c in connections) {
				
					if (connections[c].inputWidget==$(this).attr('rel')&&(connections[c].deleted==0)) {
						$(".connection"+c).remove();
						drawConnection(c);
					}
					
					if (connections[c].outputWidget==$(this).attr('rel')&&(connections[c].deleted==0)) {
					
						$(".connection"+c).remove();
						drawConnection(c);
					}
					
				}
				redrawLines();
			
			}}
		);
		
		
		$(".canvas div.widget").click(function() {
			selectedWidget = $(this).attr('id');
			$(".widgetcenter").removeClass("ui-state-highlight");
			$(this).find(".widgetcenter").addClass("ui-state-highlight");
			selectedConnection=-1;
			$(".connection").removeClass("selectedline");
			
			$(".drawingcanvases").each(function() {
			connectionid = $(this).attr('rel');	
			currentLeft = $(this).css('left');
			currentTop=$(this).css('top');		
			drawConnection(connectionid);		
			$(this).css('top',currentTop);
			$(this).css('left',currentLeft);			
			});
			//clicking on a widget selects it and deselects any connection
		});
		
		
		$(".canvas div.widget div.input").click(function() {
			selectedInput = $(this).attr('id');
			
			$(".input").removeClass("ui-state-highlight");
			$(this).addClass("ui-state-highlight");
			
			if (selectedOutput!="") {			
				addConnection(selectedOutput,selectedInput);
				selectedInput="";
				selectedOutput="";
				$(".input").removeClass("ui-state-highlight");
				$(".output").removeClass("ui-state-highlight");				
			}
		});
		
		$(".canvas div.widget div.output").click(function() {
			selectedOutput = $(this).attr('id');
			
			$(".output").removeClass("ui-state-highlight");
			$(this).addClass("ui-state-highlight");
			if (selectedInput!="") {		
				addConnection(selectedOutput,selectedInput);
				selectedInput="";
				selectedOutput="";
				$(".input").removeClass("ui-state-highlight");
				$(".output").removeClass("ui-state-highlight");
			}				
		});
}

function importXmlWebserviceRecursive(xml) {

				if ($(xml).find('webservice').size()>0) {
				
				tempxml = xml;
				xml = $(xml).find('webservice').eq(0);
							
				impDialogMarkup = '<div id="importwebservice'+importId+'" class="importdialog" title="'+$(xml).find('operation').text()+' import">';
				impDialogMarkup = impDialogMarkup+'<form name="importform'+importId+'">';
				impDialogMarkup = impDialogMarkup+'<fieldset>';
				$(xml).find('input').each(function() {
					impDialogMarkup = impDialogMarkup+'<label for="import-'+$(this).text()+'">'+$(this).text()+'</label>';
					if (($(this).attr('values'))=="") {
						impDialogMarkup = impDialogMarkup+'<input class="'+$(this).text()+'-'+importId+'" name="import-'+importId+'-'+$(this).text()+'" style="width:30px;display:inline;" type="radio" value="input" checked /> Input <input class="'+$(this).text()+'-'+importId+'" name="import-'+importId+'-'+$(this).text()+'" style="width:30px;display:inline;" type="radio" value="preference" /> Preference<div class="clear"><!----></div>';
					} else {
						impDialogMarkup = impDialogMarkup+'<input class="'+$(this).text()+'-'+importId+'" name="import-'+importId+'-'+$(this).text()+'" style="width:30px;display:inline;" type="radio" value="input" /> Input <input class="'+$(this).text()+'-'+importId+'" name="import-'+importId+'-'+$(this).text()+'" style="width:30px;display:inline;" type="radio" value="preference" checked /> Preference<div class="clear"><!----></div>';
					}
				});
				
				$(xml).find('input').each(function() {
					//impDialogMarkup = impDialogMarkup+'<input type="hidden"';
				});
				
				impDialogMarkup = impDialogMarkup+'</fieldset>';
				impDialogMarkup = impDialogMarkup+'</form>';
				impDialogMarkup = impDialogMarkup+'</div>';
					
				$("#dialogs").append(impDialogMarkup);							
				
				$("#importwebservice"+importId).dialog({autoOpen: false,
			modal: true,
			resizable: false,
			buttons: {
			"Import": function() {
			
				importPrefs = new Array();
				importInputs = new Array();
				importOutputs = new Array();
				
				importInputsCounter = 0;
				importOutputsCounter = 0;
			
				$(xml).find('input').each(function() {
					
					value = $("input."+$(this).text()+'-'+(importId-1)+":checked").val();
					if (value=='input') {
						importInputs.push("<inputws type='all' widgetLink='undefined' title='"+$(this).text()+"' required='false'>"+$(this).text().substring(0,3)+"</inputws>");
						importPrefs.push("<pref name='inp"+importInputsCounter+"' type='hidden' defaultvalue='"+$(this).text()+"'></pref>");
						importInputsCounter++;
					} else {
						if ($(this).attr('values')!="") {
							importPrefs.push("<pref name='wsinput-"+$(this).text()+"' type='select' defaultvalue='' values='"+$(this).attr('values')+"'>"+$(this).text()+"</pref>");
						} else {
							importPrefs.push("<pref name='wsinput-"+$(this).text()+"' type='text' defaultvalue=''>"+$(this).text()+"</pref>");
						}
					}
				
				});
				//$(this).dialog("close");
				
				importPrefs.push("<pref name='wsdlurl' defaultvalue='"+wsdl+"' type='text'>WSDL URL</pref>");
				importPrefs.push("<pref name='encoding' defaultvalue='' type='select' values='ISO-8859-1,UTF-8'>Encoding</pref>");
				importPrefs.push("<pref name='wsoperation' defaultvalue='"+$(xml).find('operation').text()+"' type='hidden'></pref>");
				
				
				$(xml).find('output').each(function() {
					importOutputs.push("<output type='all' title='"+$(this).text()+"' widgetLink='undefined'>"+$(this).text().substring(0,3)+"</output>");
					importPrefs.push("<pref name='out"+importOutputsCounter+"' type='hidden' defaultvalue='"+$(this).text()+"'></pref>");
					importOutputsCounter++;
				});
				
				
				//START BUILDING
				
				newxml = '<?xml version="1.0" encoding="utf-8"?><workflow><widgets><widget><top>0px</top><left>0px</left><id>0</id><name>'+$(xml).find('operation').text()+'</name><icon>WebService.png</icon><preferences>';
				
				for (iP in importPrefs) {
					newxml += importPrefs[iP];
				}
				
				newxml += '</preferences><inputs>';
				
				for (iI in importInputs) {
					newxml += importInputs[iI];
				}
				
				newxml += '</inputs><outputs>';
				
				for (iO in importOutputs) {
					newxml += importOutputs[iO];
				}
				
				newxml += '</outputs><action>callwebservice.php</action><canvas>0</canvas><counter></counter><type>0</type><deleted>0</deleted></widget></widgets><connections></connections></workflow>';
				
				loadedUserWidgets[loadedUserWidgetsCounter]=newxml;
				name = $(newxml).find("widget").eq(0).find("name").text()
				$("#userwidgets").append('<li><a class="wid userwidget" rel="'+loadedUserWidgetsCounter+'">'+name+'</a></li>');
				loadedUserWidgetsCounter++;
				$(".userwidget").eq($(".userwidget").size()-1).click(function() {
					userxml = loadedUserWidgets[$(this).attr('rel')];
					//alert(userxml);
					conversionTable = new Array();
					$(userxml).find('widget').each(function(){
						id = $(this).children('id').text();
						conversionTable[id]=activeWidgetsCounter;
						activeWidgetsCounter++;
					});
					var restoreCanvasId=activeCanvasId;
					var restoreCanvas=activeCanvas;
					var first=true;
					$(userxml).find('widget').each(function(){
						id = $(this).children('id').text();
						i = 0;
						inputs = new Array();
						outputs = new Array();
						prefs = new Array();
						$(this).find('inputws').each(function() {
							/*alert($(this).text());
							alert($(this).attr('type'));*/
							inputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
							inputs[i].title=$(this).attr('title');
							if ($(this).attr('widgetLink')!="undefined") {
								inputs[i].widgetLink=conversionTable[$(this).attr('widgetLink')];
							}
							i++;
						});
						subInputCounters[conversionTable[id]]=i;
						i=0;
						$(this).find('output').each(function() {
							outputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
							outputs[i].title=$(this).attr('title');
							if ($(this).attr('widgetLink')!="undefined") {
								outputs[i].widgetLink=$(this).attr('widgetLink');
							}
							i++;
						});
						subOutputCounters[conversionTable[id]]=i;
						i=0;
						$(this).find('pref').each(function() {
							prefs[i]=new Preference($(this).text(),$(this).attr('type'),$(this).attr('name'),$(this).attr('defaultvalue'));
							prefs[i].values=$(this).attr('values');
							i++;
						});
						activeWidgets[conversionTable[id]]=new Widget($(this).children('name').text(),$(this).children('icon').text(),inputs,outputs,prefs,$(this).children('action').text());
						if (first) {
							activeWidgets[conversionTable[id]].canvas=activeCanvasId;
						} else {
							activeWidgets[conversionTable[id]].canvas=conversionTable[$(this).children('canvas').text()];
						}
						activeWidgets[conversionTable[id]].counter=$(this).children('counter').text();
						activeWidgets[conversionTable[id]].type=$(this).children('type').text();
						activeWidgets[conversionTable[id]].deleted=$(this).children('deleted').text();
						activeWidgets[conversionTable[id]].action=$(this).children('action').text();
						//alert($(this).children('action').text());
						if (activeCanvasId!=activeWidgets[conversionTable[id]].canvas) {
							activeCanvasId = activeWidgets[conversionTable[id]].canvas;
							activeCanvas = $(".canvas"+activeCanvasId);
							resizeCanvas();
						}
						widget = activeWidgets[conversionTable[id]];
						widgetMarkup = '<div id="widget'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widget"><div class="inputs">';
						for (i in widget.inputs) {
							widgetMarkup = widgetMarkup+'<div class="input ui-state-default ui-corner-left" rel="'+widget.inputs[i].type+'" id="input-'+conversionTable[id]+'-'+i+'" title="'+widget.inputs[i].title+'">'+widget.inputs[i].name+'</div>';
						}
						widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+widget.icon+'" /></div><div class="outputs">';
						for (o in widget.outputs) {
							widgetMarkup = widgetMarkup+'<div class="output ui-state-default ui-corner-right" rel="'+widget.outputs[o].type+'" id="output-'+conversionTable[id]+'-'+o+'" title="'+widget.outputs[o].title+'">'+widget.outputs[o].name+'</div>';
						}			
						widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+conversionTable[id]+'">'+widget.name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
						activeCanvas.append(widgetMarkup);
						prefDialogMarkup = '<div id="widgetpreferences-'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widgetdialog" title="'+widget.name+' preferences">';
						prefDialogMarkup = prefDialogMarkup+'<fieldset>';
						var fileuploads = new Array();
						i=0;
						for (p in widget.preferences) {
							if (widget.preferences[p].type!="file") {
								
								if (widget.preferences[p].type=="select") {
								
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<select id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" name="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" style="background:#fefefe;width:99%;" class="text ui-widget-content ui-corner-all" onchange="changeState('+conversionTable[id]+',0);">';
									prefOptions = widget.preferences[p].values.split(",");
									for (pO in prefOptions) {
										prefDialogMarkup += '<option>'+prefOptions[pO]+'</option>';
									}
									prefDialogMarkup += '</select>'; 
									
								} else if (widget.preferences[p].type=="textarea") {
										prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
										prefDialogMarkup = prefDialogMarkup+'<textarea id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" name="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" onchange="changeState('+conversionTable[id]+',0);" class="text ui-widget-content ui-corner-all">'+widget.preferences[p].defaultvalue+'</textarea>';							
								} else {
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" name="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" type="'+widget.preferences[p].type+'" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+conversionTable[id]+',0);" class="text ui-widget-content ui-corner-all" />';
								}
								
							} else {
								prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
								prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" name="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" type="hidden" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+conversionTable[id]+',0);" class="text ui-widget-content ui-corner-all" />';						
								prefDialogMarkup = prefDialogMarkup+'<div id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'-uploader"></div>';
								fileuploads[i]=new Array();
								fileuploads[i][0]=conversionTable[id];
								fileuploads[i][1]=widget.preferences[p].name;
							}
						}
						prefDialogMarkup = prefDialogMarkup+'</fieldset>';
						prefDialogMarkup = prefDialogMarkup+'</div>';
						resDialogMarkup = '<div id="widgetresults-'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widgetdialog" title="'+widget.name+' results">';
						resDialogMarkup = resDialogMarkup+'<fieldset>';
						resDialogMarkup = resDialogMarkup+'<div style="padding:20px;max-height:600px;overflow:auto;" id="res-'+conversionTable[id]+'" name="res-'+conversionTable[id]+'" class="text ui-widget-content ui-corner-all">No results yet.</div>';
						resDialogMarkup = resDialogMarkup+'</fieldset>';
						resDialogMarkup = resDialogMarkup+'</div>';		
						renDialogMarkup = '<div id="widgetrename-'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widgetdialog" title="Rename the widget">';
						renDialogMarkup = renDialogMarkup+'<fieldset>';
						renDialogMarkup = renDialogMarkup+'<input type="text" class="widgetnameinput" name="widgetname'+conversionTable[id]+'" id="widgetname'+conversionTable[id]+'" value="'+widget.name+'" />';
						renDialogMarkup = renDialogMarkup+'</fieldset>';
						renDialogMarkup = renDialogMarkup+'</div>';
						$("#dialogs").append(prefDialogMarkup);
						$("#dialogs").append(resDialogMarkup);
						$("#dialogs").append(renDialogMarkup);
						for (f in fileuploads) {
							var uploader = new qq.FileUploader({
								element: document.getElementById('pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]+'-uploader'),
								action: 'phpupload.php',
								onComplete: function(someId, fileName, responseJSON){
									$('#pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]).val(fileName);
									changeState(fileuploads[f][0],0);
								}
							});
						}						
						updateWidgetListeners();
						resizeWidgets();
						checkRequirements();
						if (!first) {
							$("#widget"+conversionTable[id]).css('left',$(this).children('left').text());
							$("#widget"+conversionTable[id]).css('top',$(this).children('top').text());
						}
						if (activeWidgets[conversionTable[id]].type==1) {
							$("#tabs").append('<div rel="'+conversionTable[id]+'" class="canvas'+conversionTable[id]+' canvas" id="canvas'+conversionTable[id]+'"></div>');
							$("#tabs").tabs("add","#canvas"+conversionTable[id],activeWidgets[conversionTable[id]].name);
							subprocessCounter++;
						}
						first = false;
					});
					$(userxml).find('connection').each(function() {
						outputArray = $(this).attr('output').split("-");
						outputString = "output-"+conversionTable[outputArray[1]]+"-"+outputArray[2];
						inputArray = $(this).attr('input').split("-");
						inputString = "input-"+conversionTable[inputArray[1]]+"-"+inputArray[2];
						addConnection(outputString,inputString);
					});
					activeCanvas = restoreCanvas;
					activeCanvasId = restoreCanvasId;
				});
				
				
				
				
				
				//END BUILDING
				$(this).remove();
				$(tempxml).find('webservice').eq(0).remove();
				importXmlWebserviceRecursive(tempxml);
				
			},
			"Cancel": function() {
				$(this).remove();
				$(tempxml).find('webservice').eq(0).remove();
				importXmlWebserviceRecursive(tempxml);
			}}});
			
			$("#importwebservice"+importId).dialog('open');
							
							
						importId++;	
			} else {
				// end.
			}
}
// this function draws connections with the intention of them being easy to comprehend
function drawConnection(connectionid,bgcolor,color) {

	if (bgcolor==undefined) {
		bgcolor='#0000ff';
	}
	if (color==undefined) {
		color='rgb(173, 216, 230)';
	}
	
	if (selectedConnection==connectionid) {
		bgcolor='#ff0000';
		color='#ffaaaa';
	}

	canvasPos = activeCanvas.offset();
	connection = connections[connectionid];
	
	input = $("#"+connection.input);
	output = $("#"+connection.output);
	
	inputPos = input.offset();
	outputPos = output.offset();
	
	outputX = outputPos.left-canvasPos.left+(output.width());
	outputY = outputPos.top-canvasPos.top+(output.height()/2);
	inputX = inputPos.left-canvasPos.left;
	inputY = inputPos.top-canvasPos.top+(input.height()/2);
	
	drawingConnection = connectionid;
	/*
	jg.setColor("#aaaaaa");
	jg.setStroke(5);
	
	inputWidget = $("#widget"+connection.inputWidget);
	outputWidget = $("#widget"+connection.outputWidget);
	
	inputWidgetPos = inputWidget.position();
	outputWidgetPos = outputWidget.position();
	
	
	if (!drawStraightLines) {
	
		if (inputWidgetPos.left-outputWidgetPos.left<inputWidget.width()+70) {
		
		
		if (inputWidgetPos.top>outputWidgetPos.top||true) {
		
		halfDistance = (inputY-outputY)/2;
		jg.drawLine(outputX,outputY,outputX+30,outputY);
		jg.drawLine(outputX+30,outputY,outputX+30,outputY+halfDistance);
		jg.drawLine(outputX+30,outputY+halfDistance,inputX-30,outputY+halfDistance);
		jg.drawLine(inputX-30,outputY+halfDistance,inputX-30,inputY);
		jg.drawLine(inputX-30,inputY,inputX,inputY);
		}		
		
		} else {
		
		inputstring = input.attr('id').split('-');
		
		niceLineSpacing = inputstring[2]*29;
		
		if (inputWidgetPos.top<outputWidgetPos.top) {
		
			niceLineSpacing = niceLineSpacing*(-1);
		
		}
		
		halfDistance = ((inputX-outputX-30)/2)-niceLineSpacing;
		jg.drawLine(outputX,outputY,outputX+30,outputY);
		jg.drawLine(outputX+30,outputY,outputX+halfDistance,outputY);
		jg.drawLine(outputX+halfDistance,outputY,outputX+halfDistance,inputY);
		jg.drawLine(outputX+halfDistance,inputY,inputX-30,inputY);
		jg.drawLine(inputX-30,inputY,inputX,inputY);
		
		}
	
	} else {
	
		jg.drawLine(outputX,outputY,inputX,inputY);
	
	}
	jg.paint();
	*/
	
	
      var p1 = [outputX,outputY];
      var p2 = [inputX,inputY];
  
      var coeffMulDirection = 100;
   
   
      var distance=Math.sqrt(Math.pow(p1[0]-p2[0],2)+Math.pow(p1[1]-p2[1],2));
      if(distance < coeffMulDirection||true){
         coeffMulDirection = distance/2;
      }
   
      
      var d1 = [1*coeffMulDirection,
                0*coeffMulDirection];
      var d2 = [-1*coeffMulDirection,
                0*coeffMulDirection];
				
	  if (outputX>inputX&&Math.abs(outputY-inputY)<65) {
	  coeffMulDirection=150;
      var d1 = [1*coeffMulDirection,
                -1*coeffMulDirection];	  
      var d2 = [-1*coeffMulDirection,
                -1*coeffMulDirection];		
	  }
	  
	/*  if (outputX>inputX&&Math.abs(outputY-inputY)<50&&(outputY-inputY)<0) {
	  coeffMulDirection=150;
      var d1 = [1*coeffMulDirection,
                1*coeffMulDirection];	  
      var d2 = [-1*coeffMulDirection,
                1*coeffMulDirection];		
	  }	*/  
   
      var bezierPoints=[];
      bezierPoints[0] = p1;
      bezierPoints[1] = [p1[0]+d1[0],p1[1]+d1[1]];
      bezierPoints[2] = [p2[0]+d2[0],p2[1]+d2[1]];
      bezierPoints[3] = p2;
      var min = [p1[0],p1[1]];
      var max = [p1[0],p1[1]];
      for(var i=1 ; i<bezierPoints.length ; i++){
         var p = bezierPoints[i];
         if(p[0] < min[0]){
            min[0] = p[0];
         }
         if(p[1] < min[1]){
            min[1] = p[1];
         }
         if(p[0] > max[0]){
            max[0] = p[0];
         }
         if(p[1] > max[1]){
            max[1] = p[1];
         }
      }
      
      var margin = [4,4];
      min[0] = min[0]-margin[0];
      min[1] = min[1]-margin[1];
      max[0] = max[0]+margin[0];
      max[1] = max[1]+margin[1];
      var lw = Math.abs(max[0]-min[0]);
      var lh = Math.abs(max[1]-min[1]);
   
      /*this.SetCanvasRegion(min[0],min[1],lw,lh);*/
	  
	  if ($('#drawingcanvas'+connectionid).length == 0) {
		//$("div#canvas"+activeCanvasId+" div.canvasholder").append("<canvas rel='"+connectionid+"' class='drawingcanvases' id='drawingcanvas"+connectionid+"' width='"+lw+"' height='"+lh+"' style='position:absolute;top:"+min[1]+";left:"+min[0]+";width:"+lw+";height:"+lh+";z-index:0;'></canvas>");
		var somecanvas = document.createElement('canvas'); $(somecanvas).attr('id',"drawingcanvas"+connectionid).attr('width',lw).attr('height',lh).css('top',min[1]).css('left',min[0]).css('position','absolute').addClass("drawingcanvases").attr('rel',connectionid).appendTo($("div#canvas"+activeCanvasId+" div.canvasholder")); if($.browser.msie) somecanvas = G_vmlCanvasManager.initElement(somecanvas);
		
	  }
	  
	  $("#drawingcanvas"+connectionid).css({'left':min[0],'top':min[1],'width':lw,'height':lh});
	  $("#drawingcanvas"+connectionid).attr('width',lw);
	  $("#drawingcanvas"+connectionid).attr('height',lh);
   
      var drawingcanvas = document.getElementById('drawingcanvas'+connectionid);   
      var ctxt = drawingcanvas.getContext('2d');
	  
	  ctxt.clearRect(0,0,lw,lh);

   

      for(i = 0 ; i<bezierPoints.length ; i++){
         bezierPoints[i][0] = bezierPoints[i][0]-min[0];
         bezierPoints[i][1] = bezierPoints[i][1]-min[1];
      }
   
      // Draw the border
      ctxt.lineCap = 'round';
      ctxt.strokeStyle = bgcolor;
      ctxt.lineWidth = 3+1*2;
      ctxt.beginPath();
      ctxt.moveTo(bezierPoints[0][0],bezierPoints[0][1]);
      ctxt.bezierCurveTo(bezierPoints[1][0],bezierPoints[1][1],bezierPoints[2][0],bezierPoints[2][1],bezierPoints[3][0],bezierPoints[3][1]);
      ctxt.stroke();
   
      // Draw the inner bezier curve
      ctxt.lineCap = 'round';
      ctxt.strokeStyle = color;
      ctxt.lineWidth = 3;
      ctxt.beginPath();
      ctxt.moveTo(bezierPoints[0][0],bezierPoints[0][1]);
      ctxt.bezierCurveTo(bezierPoints[1][0],bezierPoints[1][1],bezierPoints[2][0],bezierPoints[2][1],bezierPoints[3][0],bezierPoints[3][1]);
      ctxt.stroke();
	  	
	// stop drawing on the canvas
	
	updateConnectionListeners();
	
	//setTimeout(resetSelection,100);
	
	
}
resetSelection = function() {
	selectedInput="";
	selectedOutput="";
	$(".input").removeClass("ui-state-highlight");
	$(".output").removeClass("ui-state-highlight");	
}
/* this function is necesary because the library for drawing has bugs when drawing on a surface whose scrollPosition is not on the top left.
The scroll position is set to 0,0 before drawing and restored to the original position afterwards. */
function redrawLines() {
	$(".connection").remove();
	
	savedScrollTop = activeCanvas.attr('scrollTop');
	savedScrollLeft = activeCanvas.attr('scrollLeft');
	
	activeCanvas.attr({ scrollTop: 0, scrollLeft: 0 });
	for (c in connections) {
		if (connections[c].deleted==0) {
			drawConnection(c);
		}
	}
	activeCanvas.attr({ scrollTop: savedScrollTop, scrollLeft: savedScrollLeft });
	resizeWidgets();
}
// This function adds a connection to the workflow.
function addConnection(output,input) {
	outputarray = output.split("-");
	inputarray = input.split("-");
	if (activeWidgets[outputarray[1]].canvas!=activeWidgets[inputarray[1]].canvas) {
		reportError("Cannot connect widgets from different scopes");
		setTimeout(resetSelection,100);
		return false;
	}
	if (activeWidgets[outputarray[1]].type==2) {
		activeWidgets[outputarray[1]].outputs[outputarray[2]].type=$("#"+input).attr('rel');
		$("#"+output).attr('rel',$("#"+input).attr('rel'));
		activeWidgets[activeWidgets[outputarray[1]].canvas].inputs[activeWidgets[outputarray[1]].counter].type=$("#"+input).attr('rel');
		activeWidgets[activeWidgets[outputarray[1]].canvas].inputs[activeWidgets[outputarray[1]].counter].required = activeWidgets[inputarray[1]].inputs[inputarray[2]].required;
		activeWidgets[activeWidgets[outputarray[1]].canvas].inputs[activeWidgets[outputarray[1]].counter].name = activeWidgets[inputarray[1]].inputs[inputarray[2]].name;
		$("#"+output).html($("#"+input).html());
		$("#input-"+activeWidgets[outputarray[1]].canvas+"-"+activeWidgets[outputarray[1]].counter).attr('rel',$("#"+input).attr('rel'));
		$("#input-"+activeWidgets[outputarray[1]].canvas+"-"+activeWidgets[outputarray[1]].counter).html($("#"+input).html());
	}
	
	if (activeWidgets[inputarray[1]].type==3) {
		activeWidgets[inputarray[1]].inputs[inputarray[2]].type=$("#"+output).attr('rel');
		$("#"+input).attr('rel',$("#"+output).attr('rel'));
		activeWidgets[activeWidgets[inputarray[1]].canvas].outputs[activeWidgets[inputarray[1]].counter].type=$("#"+output).attr('rel');
		activeWidgets[activeWidgets[inputarray[1]].canvas].outputs[activeWidgets[inputarray[1]].counter].name = activeWidgets[outputarray[1]].outputs[outputarray[2]].name;
		
		$("#"+input).html($("#"+output).html());
		$("#output-"+activeWidgets[inputarray[1]].canvas+"-"+activeWidgets[inputarray[1]].counter).attr('rel',$("#"+input).attr('rel'));
		$("#output-"+activeWidgets[inputarray[1]].canvas+"-"+activeWidgets[inputarray[1]].counter).html($("#"+input).html());
	}
	
	if (activeWidgets[outputarray[1]].type==1) {
		// we have to find such widget that it's id is the same as the canvas id of this widget and its output counter is the same as the output counter of this widget
		// we also have to find all other connections with this output and find out if one of the outputs is required - if so, the input of the output widget must also be required
		var required = "false";
		if (activeWidgets[inputarray[1]].inputs[inputarray[2]].required!="true") {
			for (c in connections) {
				if ((connections[c].output == output)&&(connections[c].deleted==0)) {
					otherInputArray = connections[c].input.split("-");
					if (activeWidgets[otherInputArray[1]].inputs[otherInputArray[2]].required=="true") {
						required="true";
						break;
					}
				}
			}
		} else {
			required = "true";
		}
		for (w in activeWidgets) {
			if (activeWidgets[w].deleted==0&&activeWidgets[w].canvas==outputarray[1]&&activeWidgets[w].counter==outputarray[2]&&activeWidgets[w].type==3) {
				activeWidgets[w].inputs[0].required = required;
			}
		}
	}
	if ($("#"+output).attr('rel')!=$("#"+input).attr('rel')&&($("#"+output).attr('rel')!="all")&&($("#"+input).attr('rel')!="all")) {
		reportError("Types do not match - Output: "+($("#"+output).attr('rel'))+", Input: "+($("#"+input).attr('rel')));
		setTimeout(resetSelection,100);
		return false;
	}
	
	exists = false;
	
	deletedConnection=-1;
	
	// This loop determines whether the connection already exists or not
	for (c in connections) {
		if ((connections[c].output == output)&&(connections[c].input==input)&&(connections[c].deleted==0)) {
			exists=true;
			break;
		} else if (connections[c].deleted==0) {
		
			if ((connections[c].input == input)&&(connections[c].output != output))
			{
			
				//if a different connection exists on the same INPUT it is deleted, so that it may be replaced with the new one. It is also saved, because the new one might not be valid.
				deletedConnection=c;
				connections[c].deleted = 1;
				
				$(".connection"+c).remove();
				
				$("#drawingcanvas"+c).remove();
				
			
			}
		}
	}
	if (!exists) {
	
		outputarray = output.split("-");
		inputarray = input.split("-");
	
		connections[connectionsCounter]=new Connection(output,input,outputarray[1],inputarray[1]);
		
		
		drawConnection(connectionsCounter);
		
		connectionsCounter++;
		
		//checks for cycles in the workflow after adding the connection (easier)
		if (checkForCycles(inputarray[1],inputarray[1])==false) {
			connections[connectionsCounter-1].deleted=1;
			$(".connection"+(connectionsCounter-1)).remove();
			$("#drawingcanvas"+(connectionsCounter-1)).remove();
			reportError("Adding that connection would cause a cycle in the workflow.");
			
			if (deletedConnection!=-1) {
				//restores the deleted connection which we were about to replace
				drawConnection(deletedConnection);
				connections[deletedConnection].deleted=0;
			
			}
		
		} else {
		
			// we have tampered with the input of a widget. The state of the widget is reset to 0 (has not been executed for this input)
			changeState(inputarray[1],0);
			
			reportOk("Connection added");
			
			setTimeout(resetSelection,100);
		
		}
	}
	
	checkRequirements();
	redrawLines();
	
}
function changeState(widgetId,state) {
	activeWidgets[widgetId].state=state;
	
	if (state==0) {
	
		if (activeWidgets[widgetId].canvas!=0) {
			if (activeWidgets[activeWidgets[widgetId].canvas].state!=1) {
				changeState(activeWidgets[widgetId].canvas,state);
			}
		}
	
		$("#widget"+widgetId).find(".loadingimage").attr('src','statusimages/blank.gif');
	
	}
	
	if (state==1) {
	
		$("#widget"+widgetId).find(".loadingimage").attr('src','statusimages/running.gif');
	
	}
	if (state==2) {
	
		$("#widget"+widgetId).find(".loadingimage").attr('src','statusimages/done.png');
	
	}	
}
/*
function changeWidgetState(widgetId,state) {
	activeWidgets[widgetId].state=state;
}
*/
// checkForCycles is a depth first search of the workflow starting with widgetId and searching for targetId.
function checkForCycles(widgetId,targetId) {
	noCycles = true;
	for (c in connections) {
		if (connections[c].deleted==0) {
			if (connections[c].inputWidget==widgetId) {
				if (connections[c].outputWidget==targetId) {
					noCycles=false;
				}
				noCycles = noCycles && checkForCycles(connections[c].outputWidget,targetId);
			}
		}
	}
	return noCycles;
}
// checkRequirements makes sure that all required inputs are satisfied.
function checkRequirements() {
	requirements=true;
	for (w in activeWidgets) {
		if (activeWidgets[w].deleted==0) {
			for (i in activeWidgets[w].inputs) {
				if (activeWidgets[w].inputs[i].required=='true') {
					satisfied=false;
					for (c in connections) {
						if (connections[c].deleted==0) {
							if (connections[c].input=="input-"+w+"-"+i) {
								satisfied=true;
								break;
							
							}
						}
					}
					if (!satisfied) {
						requirements=false;
						$("#input-"+w+"-"+i).addClass("ui-state-error");
					} else {
						$("#input-"+w+"-"+i).removeClass("ui-state-error");
					}
				} else {
					$("#input-"+w+"-"+i).removeClass("ui-state-error");
				}
			}
		}
	}
	return requirements;
}
function checkRequirementsOfWidget(w) {
	requirements=true;
	if (activeWidgets[w].deleted==0) {
		for (i in activeWidgets[w].inputs) {
			if (activeWidgets[w].inputs[i].required=='true') {
				satisfied=false;
				for (c in connections) {
					if (connections[c].deleted==0) {
						if (connections[c].input=="input-"+w+"-"+i) {
							satisfied=true;
							break;
						
						}
					}
				}
				if (!satisfied) {
					requirements=false;
					$("#input-"+w+"-"+i).addClass("ui-state-error");
				} else {
					$("#input-"+w+"-"+i).removeClass("ui-state-error");
				}
			}
		}
	}
	return requirements;
}
// loads widgets from the widgets.xml file, adds them into the loadedWidgets array and sets up listneres for buttons which are used to add widgets on the canvas.
// these function is to be separated
function loadWidgetsFromXML(xmlfile) {
	$.ajax({
		type: "GET",
		url: xmlfile,
		dataType: "xml",
		success: function(xml) {
			$(xml).find('folder').each(function() {
				var branches = $('<li class="closed"><span class="folder">'+$(this).attr('name')+'</span><ul></ul></li>').appendTo("#corewidgets");
				var folder = $("#corewidgets ul").last();
				$("#browser").treeview({
					add: branches
				});
			
			var someString = "";
			
			$(this).find('widget').each(function(){
				i = 0;
				inputs = new Array();
				outputs = new Array();
				prefs = new Array();
				
				$(this).find('input').each(function() {
					inputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
					i++;
				});
				
				i=0;
				$(this).find('output').each(function() {
					outputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
					i++;
				});
				
				i=0;
				$(this).find('pref').each(function() {
					prefs[i]=new Preference($(this).text(),$(this).attr('type'),$(this).attr('name'),$(this).attr('defaultvalue'));
					i++;
				});
				
				loadedWidgets[loadedWidgetsCounter]=new Widget($(this).find('name').text(),$(this).find('icon').text(),inputs,outputs,prefs,$(this).find('action').text());
				
				someString += '<li><a class="widget wid" rel="'+loadedWidgetsCounter+'"><span>'+$(this).find('name').text()+'</span></a></li>';
				loadedWidgetsCounter++;	
			});
			
			branches = $(someString).appendTo(folder);
				$("#browser").treeview({
					add: branches
				});		
			
			
			
			});
			
			//$("#widgets a.widget").button();
			
			$("#widgets a.widget").mouseenter(function() {
			
				$(this).addClass("hover");
			
			});
			
			$("#widgets a.widget").mouseleave(function() {
				$(this).removeClass("hover");
			});
			
			
			$("#widgets a.widget").click(function() { // this happens every time a new widget is put onto the canvas
				widget = loadedWidgets[$(this).attr('rel')];
				activeWidgets[activeWidgetsCounter]=new Widget(widget.name,widget.icon,widget.inputs,widget.outputs,widget.preferences,widget.action);
				activeWidgets[activeWidgetsCounter].canvas = activeCanvasId;
				widgetMarkup = '<div id="widget'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widget"><div class="inputs">';
				for (i in widget.inputs) {
					widgetMarkup = widgetMarkup+'<div class="input ui-state-default ui-corner-left" rel="'+widget.inputs[i].type+'" id="input-'+activeWidgetsCounter+'-'+i+'">'+widget.inputs[i].name+'</div>';
				}
				widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+widget.icon+'" /></div><div class="outputs">';
				for (o in widget.outputs) {
					widgetMarkup = widgetMarkup+'<div class="output ui-state-default ui-corner-right" rel="'+widget.outputs[o].type+'" id="output-'+activeWidgetsCounter+'-'+o+'">'+widget.outputs[o].name+'</div>';
				}			
				widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+activeWidgetsCounter+'">'+widget.name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
				activeCanvas.append(widgetMarkup);
				
				prefDialogMarkup = '<div id="widgetpreferences-'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widgetdialog" title="'+widget.name+' preferences">';
				prefDialogMarkup = prefDialogMarkup+'<fieldset>';
				
				var fileuploads = new Array();
				i=0;
				for (p in widget.preferences) {
					if (widget.preferences[p].type=="file") {
						prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
						prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" name="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" type="hidden" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+activeWidgetsCounter+',0);" class="text ui-widget-content ui-corner-all" />';						
						prefDialogMarkup = prefDialogMarkup+'<div id="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'-uploader"></div>';
						fileuploads[i]=new Array();
						fileuploads[i][0]=activeWidgetsCounter;
						fileuploads[i][1]=widget.preferences[p].name;
					} else {
									if (widget.preferences[p].type=="select") {
								
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<select id="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" name="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" style="background:#fefefe;width:99%;" class="text ui-widget-content ui-corner-all" onchange="changeState('+activeWidgetsCounter+',0);">';
									prefOptions = widget.preferences[p].values.split(",");
									for (pO in prefOptions) {
										prefDialogMarkup += '<option>'+prefOptions[pO]+'</option>';
									}
									prefDialogMarkup += '</select>'; 
									
								} else if (widget.preferences[p].type=="textarea") {
										prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
										prefDialogMarkup = prefDialogMarkup+'<textarea id="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" name="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" onchange="changeState('+activeWidgetsCounter+',0);" class="text ui-widget-content ui-corner-all">'+widget.preferences[p].defaultvalue+'</textarea>';							
								} else {
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" name="pref-'+activeWidgetsCounter+'-'+widget.preferences[p].name+'" type="'+widget.preferences[p].type+'" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+activeWidgetsCounter+',0);" class="text ui-widget-content ui-corner-all" />';
								}		
					}
				}
						
				
				
				
				prefDialogMarkup = prefDialogMarkup+'</fieldset>';
				prefDialogMarkup = prefDialogMarkup+'</div>';
				
				resDialogMarkup = '<div id="widgetresults-'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widgetdialog" title="'+widget.name+' results">';
				resDialogMarkup = resDialogMarkup+'<fieldset>';
				resDialogMarkup = resDialogMarkup+'<div style="padding:20px;max-height:600px;overflow:auto;" id="res-'+activeWidgetsCounter+'" name="res-'+activeWidgetsCounter+'" class="text ui-widget-content ui-corner-all">No results yet.</div>';
				resDialogMarkup = resDialogMarkup+'</fieldset>';
				resDialogMarkup = resDialogMarkup+'</div>';				
				
				$("#dialogs").append(prefDialogMarkup);
				$("#dialogs").append(resDialogMarkup);
				
				for (f in fileuploads) {
					var uploader = new qq.FileUploader({
						element: document.getElementById('pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]+'-uploader'),
						action: 'phpupload.php',
						onComplete: function(someId, fileName, responseJSON){
							$('#pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]).val(fileName);
							changeState(fileuploads[f][0],0);
						}
					});
				}	
				
				renDialogMarkup = '<div id="widgetrename-'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widgetdialog" title="Rename the widget">';
				renDialogMarkup = renDialogMarkup+'<fieldset>';
				renDialogMarkup = renDialogMarkup+'<input type="text" class="widgetnameinput" name="widgetname'+activeWidgetsCounter+'" id="widgetname'+activeWidgetsCounter+'" value="'+activeWidgets[activeWidgetsCounter].name+'" />';
				renDialogMarkup = renDialogMarkup+'</fieldset>';
				renDialogMarkup = renDialogMarkup+'</div>';	
				
				$("#dialogs").append(renDialogMarkup);
				
				updateWidgetListeners();
				resizeWidgets();
				activeWidgetsCounter++;
				checkRequirements();
			});
		}
	});
}
function deleteWidget(widgetId) {
		
		activeWidgets[widgetId].deleted=1;
		if (activeWidgets[widgetId].type==1) {
			var i = 0;
			$("div#tabs ul li").each(function() {
				if ($(this).children("a").attr('href')=="#canvas"+widgetId) {
					$("#tabs").tabs("remove",i);
				}
				i++;
			
			});
			for (w in activeWidgets) {
				if (activeWidgets[w].canvas==widgetId) {
					deleteWidget(w);
				}
			}
		}
		if(activeWidgets[widgetId].type==2) {
		
			activeWidgets[activeWidgets[widgetId].canvas].inputs[activeWidgets[widgetId].counter].required='false';
			activeWidgets[activeWidgets[widgetId].canvas].inputs[activeWidgets[widgetId].counter].type='all';
			$("#input-"+activeWidgets[widgetId].canvas+"-"+activeWidgets[widgetId].counter).remove();
			
			for (c in connections) {
					
						if (connections[c].deleted==0) {
							if (connections[c].input=="input-"+activeWidgets[widgetId].canvas+"-"+activeWidgets[widgetId].counter) {
								connections[c].deleted=1;
								//activeWidgets[connections[c].inputWidget].state=0;
								$(".connection"+c).remove();
								$("#drawingcanvas"+c).remove();
							}
						
						}
					
					}			
			
		}
		
		
		if(activeWidgets[widgetId].type==3) {
		
			activeWidgets[activeWidgets[widgetId].canvas].outputs[activeWidgets[widgetId].counter].required='false';
			activeWidgets[activeWidgets[widgetId].canvas].outputs[activeWidgets[widgetId].counter].type='all';
			$("#output-"+activeWidgets[widgetId].canvas+"-"+activeWidgets[widgetId].counter).remove();
			
			for (c in connections) {
					
						if (connections[c].deleted==0) {
							if (connections[c].output=="output-"+activeWidgets[widgetId].canvas+"-"+activeWidgets[widgetId].counter) {
								connections[c].deleted=1;
								//activeWidgets[connections[c].inputWidget].state=0;
								$(".connection"+c).remove();
								$("#drawingcanvas"+c).remove();
							}
						
						}
					
					}			
			
		}		
		
		$("#widget"+widgetId).remove();
		
		for (c in connections) {
		
			if (connections[c].deleted==0) {
				if (connections[c].inputWidget==widgetId||connections[c].outputWidget==widgetId) {
					connections[c].deleted=1;
					//activeWidgets[connections[c].inputWidget].state=0;
					changeState(connections[c].inputWidget,0);
					$(".connection"+c).remove();
					$("#drawingcanvas"+c).remove();
					setFollowingWidgetsStates(connections[c].inputWidget,0);
				}
			
			}
		
		}
		
		reportOk("Widget deleted.");
}
// deletes a connection or a widget, depending on the selection
function deleteSelected() {
	if (selectedWidget==""&&selectedConnection==-1) {
	
		reportError("Nothing to delete.");
	
	} else if (selectedWidget!="") {
	
		
		widgetId = $("#"+selectedWidget).attr('rel');
		
		deleteWidget(widgetId);
		selectedWidget="";
	
	} else if (selectedConnection!=-1) {
	
		connections[selectedConnection].deleted=1;	
		//activeWidgets[connections[selectedConnection].inputWidget].state=0;
		changeState(connections[selectedConnection].inputWidget,0);
		setFollowingWidgetsStates(connections[selectedConnection].inputWidget,0);
		$(".connection"+selectedConnection).remove();
		$("#drawingcanvas"+selectedConnection).remove();
		selectedConnection=-1;
		reportOk("Connection deleted.");
		
	}
	
	checkRequirements();
}
function widgetFinishedCallback(widgetId,runChildren,canvasId) {
	//activeWidgets[widgetId].state=2;
	changeState(widgetId,2);
	activeWidgets[widgetId].error=0;
	$("#widget"+widgetId).children(".widgetcenter").removeClass("ui-state-error");
	for (w in preconditions) {
		for (prec in preconditions[w]) {
			if (preconditions[w][prec]==widgetId) {
				preconditions[w].splice(prec,1);
			}
		}
	}
	for (w in activeWidgets) {
		if (activeWidgets[w].deleted==0&&activeWidgets[w].state==0&&activeWidgets[w].canvas==canvasId) {
			satisfied = true;
			if (preconditions[w].length==0) {
				satisfied=true;
			} else {
			
				for (prec in preconditions[w]) {
					if (activeWidgets[preconditions[w][prec]].state!=2) {
						satisfied=false;
					}
				}
				
			}
			if (satisfied&&runChildren) {
				runWidget(w,true);
			}
		}
	}
}
function runWidget(widgetId,runChildren) {
	$("#widget"+widgetId).children(".widgetcenter").addClass("ui-state-error");
	//activeWidgets[widgetId].setState=1;
	changeState(widgetId,1);
	
	//alert(activeWidgets[widgetId].action);
	
	postdata = "something=test";
	
	for (p in activeWidgets[widgetId].preferences) {
	
		postdata = postdata+"&";
		postdata = postdata+escape(activeWidgets[widgetId].preferences[p].name)+"="+escape($("#pref-"+widgetId+"-"+activeWidgets[widgetId].preferences[p].name).val());
	
	}
	
	for (c in connections) {
		if(connections[c].deleted==0&&connections[c].inputWidget==widgetId) {
			postdata = postdata+"&";
			
			inputarray = connections[c].input.split("-");
			outputarray = connections[c].output.split("-");
			inputid = inputarray[inputarray.length-1];
			outputid = outputarray[outputarray.length-1];
			
			postdata = postdata+escape(("input"+inputid))+"="+escape(activeWidgets[connections[c].outputWidget].outputdata[outputid]);
		}
	}
	
	setFollowingWidgetsStates(widgetId,0);
	
	if (activeWidgets[widgetId].type==0) {
	$.ajax({
		url: "widgets/"+activeWidgets[widgetId].action,
		type: "POST",
		data: postdata,
		success: function(data) {
			splitdata = data.split("|");
			if (splitdata[0]=="OK"||splitdata[0]=="RESULT") {
				for (i=0;i<activeWidgets[widgetId].outputs.length;i++) {
					activeWidgets[widgetId].outputdata[i]=splitdata[i+1];
					//alert(splitdata[i+1]);
				}
				$("#res-"+widgetId).html(data);
				if (splitdata[0]=="RESULT") {
					$("#res-"+widgetId).html(splitdata[1]);
					$("#widgetresults-"+widgetId).dialog('open');
				}
				widgetFinishedCallback(widgetId,runChildren,activeWidgets[widgetId].canvas);
			} else {
				reportError("Error: "+data);
				$("#res-"+widgetId).html(data);
				activeWidgets[widgetId].error=1;
				changeState(widgetId,0);
			}
		},
		error: function() {
			reportError("Error: Script not found or not responding.");
			activeWidgets[widgetId].error=1;
			changeState(widgetId,0);
		}
	});
	
	} else if (activeWidgets[widgetId].type==1) {
		runWorkflow(widgetId);
		setTimeout("completionCheck("+widgetId+","+runChildren+")",100);
		return;
	} else if (activeWidgets[widgetId].type==2) {
		for (c in connections) {
			if(connections[c].deleted==0&&connections[c].input=="input-"+activeWidgets[widgetId].canvas+"-"+activeWidgets[widgetId].counter) {
				outputarray = connections[c].output.split("-");
				activeWidgets[widgetId].outputdata[0]=activeWidgets[connections[c].outputWidget].outputdata[outputarray[2]];
			}
		}
		widgetFinishedCallback(widgetId,runChildren,activeWidgets[widgetId].canvas);
	} else if (activeWidgets[widgetId].type==3) {
		for (c in connections) {
			if(connections[c].deleted==0&&connections[c].inputWidget==widgetId) {
				outputarray = connections[c].output.split("-");
				activeWidgets[activeWidgets[widgetId].canvas].outputdata[activeWidgets[widgetId].counter]=activeWidgets[outputarray[1]].outputdata[outputarray[2]];
			}
		}
		widgetFinishedCallback(widgetId,runChildren,activeWidgets[widgetId].canvas);
	} else {
		reportError("Wrong widget type!");
	}
	/* $.post("widgets/"+activeWidgets[widgetId].action, postdata,
		function(data){
		//alert("Data Loaded: " + data);
		
		widgetFinishedCallback(widgetId,runChildren);
		
	}); */
	
	//setTimeout("widgetFinishedCallback('"+widgetId+"',"+runChildren+")",3000);
}
function completionCheck(widgetId,runChildren) {
	var finished=true;
	var error=false;
	//alert("checking widget"+widgetId);
	for (w in activeWidgets) {
		if (activeWidgets[w].deleted==0&&activeWidgets[w].canvas==widgetId) {
			if (activeWidgets[w].state==1||(activeWidgets[w].error==0&&activeWidgets[w].state==0)) {
				finished=false;
			}
			if (activeWidgets[w].state==0&&activeWidgets[w].error==1) {
				error=true;
				finished=true;
				break;
			}
		}
	}
	if (finished) {
		if (error) {
			activeWidgets[widgetId].error=1;
			changeState(widgetId,0);
		} else {
			setFollowingWidgetsStates(widgetId,0);
			widgetFinishedCallback(widgetId,runChildren,activeWidgets[widgetId].canvas);
		}
	} else {
		setTimeout("completionCheck("+widgetId+","+runChildren+")",100);
	}
}
function setFollowingWidgetsStates(widgetId,state) {
	for (c in connections) {
		if (connections[c].deleted==0&&connections[c].outputWidget==widgetId) {
			changeState(connections[c].inputWidget,state);
			setFollowingWidgetsStates(connections[c].inputWidget,state);
		}
	}
}
function calculatePreconditions() {
	for (w in activeWidgets) {
		if (activeWidgets[w].deleted==0) {
			if (activeWidgets[w].inputs.length==0) {
				preconditions[w] = new Array();
			} else {
				preconditions[w] = new Array();
				var prec=0;
				for (c in connections) {
					if (connections[c].deleted==0) {
						if (connections[c].inputWidget==w) {
							if (activeWidgets[connections[c].outputWidget].state==0) {
								//activeWidgets[w].state=0;
								changeState(w,0);
							}
							preconditions[w][prec]=connections[c].outputWidget;
							prec++;
						}
					}
				}
			}
		}
	}
}
function runWorkflow(canvasId) {
	preconditions = new Array();
	if (checkRequirements()) {
		calculatePreconditions();
		
		for (w in activeWidgets) {
			if (activeWidgets[w].deleted==0&&activeWidgets[w].state==0&&activeWidgets[w].canvas==canvasId) {
				satisfied = true;
				if (preconditions[w].length==0) {
					satisfied=true;
				} else {
				
					for (prec in preconditions[w]) {
						if (activeWidgets[preconditions[w][prec]].state!=2) {
							satisfied=false;
						}
					}
					
				}
				if (satisfied) {
					runWidget(w,true);
				}
			}
		}
		
		
	
	} else {
		reportError("Required inputs unsatisfied. Please provide all required input data.");
	}
}
function resizeCanvas() {
	contentheight = $("#content").height();
	//alert(contentheight);
	activeCanvas.css('height',(contentheight-90)+"px");
}
// this is the jquery document ready function. It executes when the entire DOM is loaded
$(function(){
	$("#tabs").tabs({
	select: function(event, ui) {
		activeCanvasId = $(ui.panel).attr('rel');
		activeCanvas = $("#canvas"+activeCanvasId);
		resizeCanvas();
		resizeWidgets();
		jg = new jsGraphics("canvas"+activeCanvasId);
		setTimeout("redrawLines()",500);
	},
	tabTemplate: '<li><a href="#{href}"><span rel="#{href}">#{label}</span></a></li>'
	});
	
	activeCanvas = $(".canvas0");
	
	resizeCanvas();
	
	$(window).bind('resize', function() {
	
	resizeCanvas();
	
	});
	jg = new jsGraphics("canvas0");
	$(".expand").click(function() {
	
		//$(this).parent().children("ul").slideToggle();
	
	});
	
	$("#status").css('margin-left',''+(($("#toolbar ul#icons li").size()*30)+4)+'px');
	
	$('#icons li').hover(
		function() { $(this).addClass('ui-state-hover'); }, 
		function() { $(this).removeClass('ui-state-hover'); }
	);
	
	$(".new").click(function() {
		$('#newdialog').dialog('open');
	});
	
	$(window).keypress(function(event) {
		if (event.keyCode==46) {
			deleteSelected();
		}
	});
	
	
	
	$(".delete").click(function() {
		deleteSelected();
	});
	
	$(".redraw").click(function() {
		redrawLines();
	});	
	
	$(".preferences").click(function() {
		$('#preferencesdialog').dialog('open');
	});	
	$(".open").click(function() {
		$("#opendialog").dialog('open');
	});
	$(".loadwidget").click(function() {
		$("#loadwidgetdialog").dialog('open');
	});	
	$(".save").click(function() {
		xmlstring = '<?xml version="1.0" encoding="utf-8"?>\n';
		xmlstring += '<workflow>\n';
		xmlstring += '<widgets>\n';
		for (w in activeWidgets) {
			top = $("#widget"+w).css('top');
			left = $("#widget"+w).css('left');
			xmlstring += '  <widget>\n    <top>'+top+'</top>\n    <left>'+left+'</left>\n    <id>'+w+'</id>\n    <name>'+activeWidgets[w].name+'</name>\n    <icon>'+activeWidgets[w].icon+'</icon>\n';
			
			xmlstring += '    <preferences>\n';
			for (p in activeWidgets[w].preferences) {
				xmlstring += "    <pref type='"+activeWidgets[w].preferences[p].type+"' name='"+activeWidgets[w].preferences[p].name+"' defaultvalue='"+($("#pref-"+w+"-"+activeWidgets[w].preferences[p].name).val())+"'>"+activeWidgets[w].preferences[p].label+"</pref>\n";
			}
			xmlstring += '    </preferences>\n';
						
			xmlstring += '    <inputs>\n';
			for (i in activeWidgets[w].inputs) {
				xmlstring += "        <input type='"+activeWidgets[w].inputs[i].type+"' required='"+activeWidgets[w].inputs[i].required+"' widgetLink='"+activeWidgets[w].inputs[i].widgetLink+"'>"+activeWidgets[w].inputs[i].name+"</input>\n";
			}
			xmlstring += '    </inputs>\n';
			
			xmlstring += '    <outputs>\n';
			for (o in activeWidgets[w].outputs) {
				xmlstring += "        <output type='"+activeWidgets[w].outputs[o].type+"' widgetLink='"+activeWidgets[w].outputs[o].widgetLink+"'>"+activeWidgets[w].outputs[o].name+"</output>\n";
			}
			xmlstring += '    </outputs>\n';			
			
			xmlstring += '    <action>'+activeWidgets[w].action+'</action>\n    <canvas>'+activeWidgets[w].canvas+'</canvas>\n    <counter>'+activeWidgets[w].counter+'</counter>\n    <type>'+activeWidgets[w].type+'</type>\n    <deleted>'+activeWidgets[w].deleted+'</deleted>\n  </widget>\n'
		}
		xmlstring += '</widgets>\n';
		xmlstring += '<connections>\n';
		for (c in connections) {
			if (connections[c].deleted==0) {
				xmlstring += "  <connection output='"+connections[c].output+"' input = '"+connections[c].input+"' /> \n";
			}
		}
		xmlstring += '</connections>\n';
		xmlstring += '</workflow>\n';
		$("#workflowsave").val(xmlstring);
		document.forms.save.submit();
	});
	$(".run").click(function() {
		//reportError("Running workflows not yet supported.");
		alreadyBeenRun=false;
		for (w in activeWidgets) {
			if (activeWidgets[w].deleted==0&&activeWidgets[w].state>0) {
				alreadyBeenRun=true;
			}
		}
		if (alreadyBeenRun) {
			$("#rundialog").dialog("open");
		} else {
			runWorkflow(0);
		}
		
	});
	$(".info").click(function() {
		reportStatus("Test");
	});	
	
	$('#newdialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Yes": function() { 
				/*$(this).dialog("close"); 
				activeCanvas.html('');
				activeWidgets = new Array();
				activeWidgetsCounter = 0;
				connections = new Array();
				connectionsCounter = 0;
				selectedInput="";
				selectedOutput="";
				selectedWidget="";
				selectedConnection=-1;
				$(".input").removeClass("ui-state-highlight");
				$(".output").removeClass("ui-state-highlight");					
				reportOk("New workflow created");
				jg = new jsGraphics("canvas");
				*/
				window.location = '/new-workflow/';
			}, 
			"No": function() { 
				$(this).dialog("close"); 
			} 
		}
	});
	
	$('#wsdldialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Import": function() {
				$(this).dialog('close');
				wsdl = $("#wsdlinput").val();
					postdata= "wsdl="+escape(wsdl);
					$.ajax({
						url: "import.php",
						type: "POST",
						data: postdata,
						dataType: "xml",
						success: function(xml) {
							$(this).dialog("close");
							importXmlWebserviceRecursive(xml);
						},
						error: function(e,f) {
							reportError("Error: Cannot import webservice."+f);
							$(this).dialog("close");
						}
					});
				
			}, 
			"Cancel": function() { 
				$(this).dialog("close"); 
			} 
		}
	});	
	
	$('#rundialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Run everything": function() {
				$(this).dialog("close");
				for (w in activeWidgets) {
					if (activeWidgets[w].deleted==0) {
						//activeWidgets[w].state=0;
						changeState(w,0);
					}
				}
				runWorkflow(0);
			}, 
			"Run the changes": function() { 
				$(this).dialog("close");
				runWorkflow(0); 
			} 
		}
	});	
	
	$('#opendialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Cancel": function() {
				$(this).dialog("close");
			}, 
			"Load": function() {
				document.forms.loadworkflow.submit();
			} 
		}
	});		
	
	$('#preferencesdialog').dialog({
		autoOpen: false,
		modal: false,
		resizable: false,
		buttons: {
			"Close": function() { 
				$(this).dialog("close"); 
			} 
		}
	});	
	
	$('#preconditionsdialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Yes": function() {
				$(this).dialog("close"); 
			},		
			"No": function() {
				$(this).dialog("close"); 
			}
		}
	});
	
	$('#tempdialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: true,
		width: "1000px",
		buttons: {
			"Ok!": function() {
				$(this).dialog("close"); 
			}
		}	
	});
	
	$('#loadwidgetdialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Cancel": function() {
				$(this).dialog("close");
			} 
		}
	});	
	
	$("#browser").treeview();
	
	
	loadWidgetsFromXML('widgets/widgets.xml');

	
	
			$("#widgets a.subprocess").click(function() { // this happens when you want to add a new subprocess
				activeWidgets[activeWidgetsCounter]=new Widget("Subprocess "+subprocessCounter,"question-mark.gif",new Array(),new Array(),"","");
				activeWidgets[activeWidgetsCounter].canvas = activeCanvasId;
				activeWidgets[activeWidgetsCounter].type = 1;
				subInputCounters[activeWidgetsCounter]=0;
				subOutputCounters[activeWidgetsCounter]=0;
				widgetMarkup = '<div id="widget'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widget"><div class="inputs">';
	
				widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+activeWidgets[activeWidgetsCounter].icon+'" /></div><div class="outputs">';
				widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+activeWidgetsCounter+'">'+activeWidgets[activeWidgetsCounter].name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
				activeCanvas.append(widgetMarkup);
				$("#tabs").append('<div rel="'+activeWidgetsCounter+'" class="canvas'+activeWidgetsCounter+' canvas" id="canvas'+activeWidgetsCounter+'"><div class="canvasholder"></div></div>');
				$("#tabs").tabs("add","#canvas"+activeWidgetsCounter,activeWidgets[activeWidgetsCounter].name);
				
				
				renDialogMarkup = '<div id="widgetrename-'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widgetdialog" title="Rename the widget">';
				renDialogMarkup = renDialogMarkup+'<fieldset>';
				renDialogMarkup = renDialogMarkup+'<input type="text" class="widgetnameinput" name="widgetname'+activeWidgetsCounter+'" id="widgetname'+activeWidgetsCounter+'" value="'+activeWidgets[activeWidgetsCounter].name+'" />';
				renDialogMarkup = renDialogMarkup+'</fieldset>';
				renDialogMarkup = renDialogMarkup+'</div>';	
				
				$("#dialogs").append(renDialogMarkup);	
				
				updateWidgetListeners();
				resizeWidgets();
				activeWidgetsCounter++;
				subprocessCounter++;
				checkRequirements();
			});	
			
			$("#widgets a.input").click(function() { // this happens when you want to add an input widget
				if (activeCanvasId!=0) {
				activeWidgets[activeWidgetsCounter]=new Widget("Input "+subInputCounters[activeCanvasId],"question-mark.gif",new Array(),new Array(),"","");
				activeWidgets[activeWidgetsCounter].canvas = activeCanvasId;
				activeWidgets[activeWidgetsCounter].type = 2;
				activeWidgets[activeWidgetsCounter].counter=subInputCounters[activeCanvasId];
				widgetMarkup = '<div id="widget'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widget"><div class="inputs">';
				widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+activeWidgets[activeWidgetsCounter].icon+'" /></div><div class="outputs">';
				activeWidgets[activeWidgetsCounter].outputs[0]=new InputOutput("inp "+subInputCounters[activeCanvasId],"all",false,activeCanvasId,subInputCounters[activeCanvasId]);
				activeWidgets[activeCanvasId].inputs.push(new InputOutput("inp "+subInputCounters[activeCanvasId],"all",false,activeWidgetsCounter,subInputCounters[activeCanvasId]));
				widgetMarkup = widgetMarkup+'<div class="output ui-state-default ui-corner-right" rel="all" id="output-'+activeWidgetsCounter+'-'+0+'">inp '+subInputCounters[activeCanvasId]+'</div>';
				inputMarkup = '<div class="input ui-state-default ui-corner-left" rel="all" id="input-'+activeCanvasId+'-'+subInputCounters[activeCanvasId]+'">inp '+subInputCounters[activeCanvasId]+'</div>';
				$("#widget"+activeCanvasId+" div.inputs").append(inputMarkup);
				widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+activeWidgetsCounter+'">'+activeWidgets[activeWidgetsCounter].name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
				activeCanvas.append(widgetMarkup);
				
				
				renDialogMarkup = '<div id="widgetrename-'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widgetdialog" title="Rename the widget">';
				renDialogMarkup = renDialogMarkup+'<fieldset>';
				renDialogMarkup = renDialogMarkup+'<input type="text" class="widgetnameinput" name="widgetname'+activeWidgetsCounter+'" id="widgetname'+activeWidgetsCounter+'" value="'+activeWidgets[activeWidgetsCounter].name+'" />';
				renDialogMarkup = renDialogMarkup+'</fieldset>';
				renDialogMarkup = renDialogMarkup+'</div>';	
				
				$("#dialogs").append(renDialogMarkup);	
				updateWidgetListeners();
				resizeWidgets();
				activeWidgetsCounter++;
				subInputCounters[activeCanvasId]++;
				checkRequirements();
				} else {
					reportError("Input and output special widgets may only be put on subprocesses");
				}
			});
			
			$("#widgets a.output").click(function() { // this happens when you want to add an output widget
				if (activeCanvasId!=0) {
				activeWidgets[activeWidgetsCounter]=new Widget("Output "+subOutputCounters[activeCanvasId],"question-mark.gif",new Array(),new Array(),"","");
				activeWidgets[activeWidgetsCounter].canvas = activeCanvasId;
				activeWidgets[activeWidgetsCounter].type = 3;
				activeWidgets[activeWidgetsCounter].counter=subOutputCounters[activeCanvasId];
				widgetMarkup = '<div id="widget'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widget"><div class="inputs">';
				widgetMarkup = widgetMarkup+'<div class="input ui-state-default ui-corner-left" rel="all" id="input-'+activeWidgetsCounter+'-'+0+'">out '+subOutputCounters[activeCanvasId]+'</div>';
				widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+activeWidgets[activeWidgetsCounter].icon+'" /></div><div class="outputs">';
				activeWidgets[activeWidgetsCounter].inputs[0]=new InputOutput("out "+subOutputCounters[activeCanvasId],"all",false,activeCanvasId,subOutputCounters[activeCanvasId]);
				activeWidgets[activeCanvasId].outputs.push(new InputOutput("out "+subOutputCounters[activeCanvasId],"all",false,activeWidgetsCounter,subOutputCounters[activeCanvasId]));
				outputMarkup = '<div class="output ui-state-default ui-corner-right" rel="all" id="output-'+activeCanvasId+'-'+subOutputCounters[activeCanvasId]+'">out '+subOutputCounters[activeCanvasId]+'</div>';
				$("#widget"+activeCanvasId+" div.outputs").append(outputMarkup);
				widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+activeWidgetsCounter+'">'+activeWidgets[activeWidgetsCounter].name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
				activeCanvas.append(widgetMarkup);
				renDialogMarkup = '<div id="widgetrename-'+activeWidgetsCounter+'" rel="'+activeWidgetsCounter+'" class="widgetdialog" title="Rename the widget">';
				renDialogMarkup = renDialogMarkup+'<fieldset>';
				renDialogMarkup = renDialogMarkup+'<input type="text"  class="widgetnameinput" name="widgetname'+activeWidgetsCounter+'" id="widgetname'+activeWidgetsCounter+'" value="'+activeWidgets[activeWidgetsCounter].name+'" />';
				renDialogMarkup = renDialogMarkup+'</fieldset>';
				renDialogMarkup = renDialogMarkup+'</div>';	
				
				$("#dialogs").append(renDialogMarkup);				
				
				updateWidgetListeners();
				resizeWidgets();
				activeWidgetsCounter++;
				subOutputCounters[activeCanvasId]++;
				checkRequirements();
				} else {
					reportError("Input and output special widgets may only be put on subprocesses");
				}
			});			
	
	
	
	$("#widgets a.widget").button();
	
	resizeWidgets();
	
	
	var loadedworkflow = ""+$("#loadedworkflow").val();
	if (loadedworkflow!=""&&loadedworkflow!="undefined") {
		//alert(loadedworkflow);
		//handle loading
			var maxWidget=0;
			$.ajax({
			type: "GET",
			url: "uploads/"+loadedworkflow,
			dataType: "xml",
			success: function(xml) {
			//alert("great success");
			$(xml).find('widget').each(function(){
				id = $(this).children('id').text();
				
				if (id>maxWidget) {
					maxWidget=id;
				}
				
				i = 0;
				inputs = new Array();
				outputs = new Array();
				prefs = new Array();
				$(this).find('input').each(function() {
					inputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
					inputs[i].widgetLink=$(this).attr('widgetLink');
					i++;
				});
				subInputCounters[id]=i;
				
				
				i=0;
				$(this).find('output').each(function() {
					outputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
					outputs[i].widgetLink=$(this).attr('widgetLink');
					i++;
				});
				subOutputCounters[id]=i;
				
				i=0;
				$(this).find('pref').each(function() {
					prefs[i]=new Preference($(this).text(),$(this).attr('type'),$(this).attr('name'),$(this).attr('defaultvalue'));
					i++;
				});
				
				activeWidgets[id]=new Widget($(this).children('name').text(),$(this).children('icon').text(),inputs,outputs,prefs,$(this).children('action').text());
				activeWidgets[id].canvas=$(this).children('canvas').text();
				activeWidgets[id].counter=$(this).children('counter').text();
				activeWidgets[id].type=$(this).children('type').text();
				activeWidgets[id].deleted=$(this).children('deleted').text();
				activeWidgets[id].action=$(this).children('action').text();
				//alert($(this).children('action').text());
				
				if (activeCanvasId!=activeWidgets[id].canvas) {
					activeCanvasId = activeWidgets[id].canvas;
					activeCanvas = $(".canvas"+activeCanvasId);
					resizeCanvas();
				}
				
				widget = activeWidgets[id];
				widgetMarkup = '<div id="widget'+id+'" rel="'+id+'" class="widget"><div class="inputs">';
				for (i in widget.inputs) {
					widgetMarkup = widgetMarkup+'<div class="input ui-state-default ui-corner-left" rel="'+widget.inputs[i].type+'" id="input-'+id+'-'+i+'">'+widget.inputs[i].name+'</div>';
				}
				widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+widget.icon+'" /></div><div class="outputs">';
				for (o in widget.outputs) {
					widgetMarkup = widgetMarkup+'<div class="output ui-state-default ui-corner-right" rel="'+widget.outputs[o].type+'" id="output-'+id+'-'+o+'">'+widget.outputs[o].name+'</div>';
				}
				widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+id+'">'+widget.name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
				activeCanvas.append(widgetMarkup);
				
				prefDialogMarkup = '<div id="widgetpreferences-'+id+'" rel="'+id+'" class="widgetdialog" title="'+widget.name+' preferences">';
				prefDialogMarkup = prefDialogMarkup+'<fieldset>';
				
				var fileuploads = new Array();
				i=0;
				for (p in widget.preferences) {
					if (widget.preferences[p].type!="file") {
									if (widget.preferences[p].type=="select") {
								
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<select id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" style="background:#fefefe;width:99%;" class="text ui-widget-content ui-corner-all" onchange="changeState('+id+',0);">';
									prefOptions = widget.preferences[p].values.split(",");
									for (pO in prefOptions) {
										prefDialogMarkup += '<option>'+prefOptions[pO]+'</option>';
									}
									prefDialogMarkup += '</select>'; 
									
								} else if (widget.preferences[p].type=="textarea") {
										prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
										prefDialogMarkup = prefDialogMarkup+'<textarea id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" onchange="changeState('+id+',0);" class="text ui-widget-content ui-corner-all">'+widget.preferences[p].defaultvalue+'</textarea>';							
								} else {
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" type="'+widget.preferences[p].type+'" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+id+',0);" class="text ui-widget-content ui-corner-all" />';
								}				
					
					
						} else {
						prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
						prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" type="hidden" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+id+',0);" class="text ui-widget-content ui-corner-all" />';						
						prefDialogMarkup = prefDialogMarkup+'<div id="pref-'+id+'-'+widget.preferences[p].name+'-uploader"></div>';
						fileuploads[i]=new Array();
						fileuploads[i][0]=id;
						fileuploads[i][1]=widget.preferences[p].name;
					}
				}
				prefDialogMarkup = prefDialogMarkup+'</fieldset>';
				prefDialogMarkup = prefDialogMarkup+'</div>';
				
				resDialogMarkup = '<div id="widgetresults-'+id+'" rel="'+id+'" class="widgetdialog" title="'+widget.name+' results">';
				resDialogMarkup = resDialogMarkup+'<fieldset>';
				resDialogMarkup = resDialogMarkup+'<div style="padding:20px;max-height:600px;overflow:auto;" id="res-'+id+'" name="res-'+id+'" class="text ui-widget-content ui-corner-all">No results yet.</div>';
				resDialogMarkup = resDialogMarkup+'</fieldset>';
				resDialogMarkup = resDialogMarkup+'</div>';	
				renDialogMarkup = '<div id="widgetrename-'+id+'" rel="'+id+'" class="widgetdialog" title="Rename the widget">';
				renDialogMarkup = renDialogMarkup+'<fieldset>';
				renDialogMarkup = renDialogMarkup+'<input type="text"  class="widgetnameinput" name="widgetname'+id+'" id="widgetname'+id+'" value="'+widget.name+'" />';
				renDialogMarkup = renDialogMarkup+'</fieldset>';
				renDialogMarkup = renDialogMarkup+'</div>';		
				
				$("#dialogs").append(prefDialogMarkup);
				$("#dialogs").append(resDialogMarkup);
				$("#dialogs").append(renDialogMarkup);
				
				for (f in fileuploads) {
					var uploader = new qq.FileUploader({
						element: document.getElementById('pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]+'-uploader'),
						action: 'phpupload.php',
						onComplete: function(someId, fileName, responseJSON){
							$('#pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]).val(fileName);
							changeState(fileuploads[f][0],0);
						}
					});
				}
								
				updateWidgetListeners();
				resizeWidgets();
				checkRequirements();
				
				$("#widget"+id).css('left',$(this).children('left').text());
				$("#widget"+id).css('top',$(this).children('top').text());
				
				if (activeWidgets[id].type==1) {
				
					$("#tabs").append('<div rel="'+id+'" class="canvas'+id+' canvas" id="canvas'+id+'"></div>');
					$("#tabs").tabs("add","#canvas"+id,activeWidgets[id].name);
					subprocessCounter++;
					
				
				}
				
				
			});
			activeWidgetsCounter = maxWidget+1;
			
			
			$(xml).find('connection').each(function() {
			
				addConnection($(this).attr('output'),$(this).attr('input'));
			
			});
			
			
		},
		error: function(error,errormessage) {
			alert(errormessage);
		}
	});
		
		
		
	}
	
	var uploader = new qq.FileUploader({
        element: document.getElementById('loadwidgetuploader'),
        action: 'phpupload.php',
		onComplete: function(id, fileName, responseJSON){
		$("#loadwidgetdialog").dialog("close");
		$.ajax({
			type: "GET",
			url: "uploads/"+fileName,
			dataType: "xml",
			success: function(xml) {
				loadedUserWidgets[loadedUserWidgetsCounter]=xml;
				name = $(xml).find("widget").eq(0).find("name").text()
				$("#userwidgets").append('<li><a class="wid userwidget" rel="'+loadedUserWidgetsCounter+'">'+name+'</a></li>');
				loadedUserWidgetsCounter++;
				$(".userwidget").click(function() {
					userxml = loadedUserWidgets[$(this).attr('rel')];
					conversionTable = new Array();
					$(userxml).find('widget').each(function(){
						id = $(this).children('id').text();
						conversionTable[id]=activeWidgetsCounter;
						activeWidgetsCounter++;
					});
					var restoreCanvasId=activeCanvasId;
					var restoreCanvas=activeCanvas;
					var first=true;
					$(userxml).find('widget').each(function(){
						id = $(this).children('id').text();
						i = 0;
						inputs = new Array();
						outputs = new Array();
						prefs = new Array();
						$(this).find('input').each(function() {
							inputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
							if ($(this).attr('widgetLink')!="undefined") {
								inputs[i].widgetLink=conversionTable[$(this).attr('widgetLink')];
							}
							i++;
						});
						subInputCounters[conversionTable[id]]=i;
						i=0;
						$(this).find('output').each(function() {
							outputs[i]=new InputOutput($(this).text(),$(this).attr('type'),$(this).attr('required'));
							if ($(this).attr('widgetLink')!="undefined") {
								outputs[i].widgetLink=$(this).attr('widgetLink');
							}
							i++;
						});
						subOutputCounters[conversionTable[id]]=i;
						i=0;
						$(this).find('pref').each(function() {
							prefs[i]=new Preference($(this).text(),$(this).attr('type'),$(this).attr('name'),$(this).attr('defaultvalue'));
							i++;
						});
						activeWidgets[conversionTable[id]]=new Widget($(this).children('name').text(),$(this).children('icon').text(),inputs,outputs,prefs,$(this).children('action').text());
						if (first) {
							activeWidgets[conversionTable[id]].canvas=activeCanvasId;
						} else {
							activeWidgets[conversionTable[id]].canvas=conversionTable[$(this).children('canvas').text()];
						}
						activeWidgets[conversionTable[id]].counter=$(this).children('counter').text();
						activeWidgets[conversionTable[id]].type=$(this).children('type').text();
						activeWidgets[conversionTable[id]].deleted=$(this).children('deleted').text();
						activeWidgets[conversionTable[id]].action=$(this).children('action').text();
						//alert($(this).children('action').text());
						if (activeCanvasId!=activeWidgets[conversionTable[id]].canvas) {
							activeCanvasId = activeWidgets[conversionTable[id]].canvas;
							activeCanvas = $(".canvas"+activeCanvasId);
							resizeCanvas();
						}
						widget = activeWidgets[conversionTable[id]];
						widgetMarkup = '<div id="widget'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widget"><div class="inputs">';
						for (i in widget.inputs) {
							widgetMarkup = widgetMarkup+'<div class="input ui-state-default ui-corner-left" rel="'+widget.inputs[i].type+'" id="input-'+conversionTable[id]+'-'+i+'">'+widget.inputs[i].name+'</div>';
						}
						widgetMarkup = widgetMarkup+'</div><div class="ui-widget-content ui-corner-all widgetcenter"><img src="/static/widget-icons/'+widget.icon+'" /></div><div class="outputs">';
						for (o in widget.outputs) {
							widgetMarkup = widgetMarkup+'<div class="output ui-state-default ui-corner-right" rel="'+widget.outputs[o].type+'" id="output-'+conversionTable[id]+'-'+o+'">'+widget.outputs[o].name+'</div>';
						}			
						widgetMarkup = widgetMarkup+'</div><div class="clear"><!----></div><div style="text-align:center;" id="widgetcaption'+conversionTable[id]+'">'+widget.name+'<br /><img src="/static/statusimages/blank.gif" style="height:16px;width:16px;" class="loadingimage" /></div></div>';
						activeCanvas.append(widgetMarkup);
						prefDialogMarkup = '<div id="widgetpreferences-'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widgetdialog" title="'+widget.name+' preferences">';
						prefDialogMarkup = prefDialogMarkup+'<fieldset>';
						var fileuploads = new Array();
						i=0;
						for (p in widget.preferences) {
							if (widget.preferences[p].type!="file") {
									if (widget.preferences[p].type=="select") {
								
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<select id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" style="background:#fefefe;width:99%;" class="text ui-widget-content ui-corner-all" onchange="changeState('+id+',0);">';
									prefOptions = widget.preferences[p].values.split(",");
									for (pO in prefOptions) {
										prefDialogMarkup += '<option>'+prefOptions[pO]+'</option>';
									}
									prefDialogMarkup += '</select>'; 
									
								} else if (widget.preferences[p].type=="textarea") {
										prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
										prefDialogMarkup = prefDialogMarkup+'<textarea id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" onchange="changeState('+id+',0);" class="text ui-widget-content ui-corner-all">'+widget.preferences[p].defaultvalue+'</textarea>';							
								} else {
									prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+id+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
									prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+id+'-'+widget.preferences[p].name+'" name="pref-'+id+'-'+widget.preferences[p].name+'" type="'+widget.preferences[p].type+'" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+id+',0);" class="text ui-widget-content ui-corner-all" />';
								}									} else {
								prefDialogMarkup = prefDialogMarkup+'<label for="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'">'+widget.preferences[p].label+'</label>';
								prefDialogMarkup = prefDialogMarkup+'<input id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" name="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'" type="hidden" value="'+widget.preferences[p].defaultvalue+'" onchange="changeState('+conversionTable[id]+',0);" class="text ui-widget-content ui-corner-all" />';						
								prefDialogMarkup = prefDialogMarkup+'<div id="pref-'+conversionTable[id]+'-'+widget.preferences[p].name+'-uploader"></div>';
								fileuploads[i]=new Array();
								fileuploads[i][0]=conversionTable[id];
								fileuploads[i][1]=widget.preferences[p].name;
							}
						}
						prefDialogMarkup = prefDialogMarkup+'</fieldset>';
						prefDialogMarkup = prefDialogMarkup+'</div>';
						resDialogMarkup = '<div id="widgetresults-'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widgetdialog" title="'+widget.name+' results">';
						resDialogMarkup = resDialogMarkup+'<fieldset>';
						resDialogMarkup = resDialogMarkup+'<div style="padding:20px;max-height:600px;overflow:auto;" id="res-'+conversionTable[id]+'" name="res-'+conversionTable[id]+'" class="text ui-widget-content ui-corner-all">No results yet.</div>';
						resDialogMarkup = resDialogMarkup+'</fieldset>';
						resDialogMarkup = resDialogMarkup+'</div>';		
						renDialogMarkup = '<div id="widgetrename-'+conversionTable[id]+'" rel="'+conversionTable[id]+'" class="widgetdialog" title="Rename the widget">';
						renDialogMarkup = renDialogMarkup+'<fieldset>';
						renDialogMarkup = renDialogMarkup+'<input type="text" class="widgetnameinput" name="widgetname'+conversionTable[id]+'" id="widgetname'+conversionTable[id]+'" value="'+widget.name+'" />';
						renDialogMarkup = renDialogMarkup+'</fieldset>';
						renDialogMarkup = renDialogMarkup+'</div>';
						$("#dialogs").append(prefDialogMarkup);
						$("#dialogs").append(resDialogMarkup);
						$("#dialogs").append(renDialogMarkup);
						for (f in fileuploads) {
							var uploader = new qq.FileUploader({
								element: document.getElementById('pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]+'-uploader'),
								action: 'phpupload.php',
								onComplete: function(someId, fileName, responseJSON){
									$('#pref-'+fileuploads[f][0]+'-'+fileuploads[f][1]).val(fileName);
									changeState(fileuploads[f][0],0);
								}
							});
						}						
						updateWidgetListeners();
						resizeWidgets();
						checkRequirements();
						if (!first) {
							$("#widget"+conversionTable[id]).css('left',$(this).children('left').text());
							$("#widget"+conversionTable[id]).css('top',$(this).children('top').text());
						}
						if (activeWidgets[conversionTable[id]].type==1) {
							$("#tabs").append('<div rel="'+conversionTable[id]+'" class="canvas'+conversionTable[id]+' canvas" id="canvas'+conversionTable[id]+'"></div>');
							$("#tabs").tabs("add","#canvas"+conversionTable[id],activeWidgets[conversionTable[id]].name);
							subprocessCounter++;
						}
						first = false;
					});
					$(userxml).find('connection').each(function() {
						outputArray = $(this).attr('output').split("-");
						outputString = "output-"+conversionTable[outputArray[1]]+"-"+outputArray[2];
						inputArray = $(this).attr('input').split("-");
						inputString = "input-"+conversionTable[inputArray[1]]+"-"+inputArray[2];
						addConnection(outputString,inputString);
					});
					activeCanvas = restoreCanvas;
					activeCanvasId = restoreCanvasId;
				});
			},
			error: function(error,errormessage) {
				alert(errormessage);
			}
		});
		}
    });
	$(".importWebservice").button();
	$(".importWebservice").click(function() {
		$("#wsdldialog").dialog('open');
	});
});