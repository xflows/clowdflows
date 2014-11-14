function getCurrentTimeAsString() {
    var currentTime = new Date();
    var hours = currentTime.getHours();
    var minutes = currentTime.getMinutes();
    var seconds = currentTime.getSeconds();

    if (minutes < 10)
    { minutes = "0" + minutes; }

    if (seconds < 10)
        { seconds = "0" + seconds; }

    return hours + ":" +minutes+":"+seconds;

}


function reportError(errorMessage) {
	$("#status").find(".infotext").html(errorMessage);
	$("#status").find(".ui-icon").addClass("ui-icon-alert");
	$("#status").find(".ui-icon").removeClass("ui-icon-info");
	$("#status").find(".ui-icon").removeClass("ui-icon-circle-check");
	$("#status").removeClass("ui-state-highlight");
	$("#status").addClass("ui-state-error");
    $(".ajax-loader").hide();
    logging_val = $("#logging textarea").val();
    logging_val += "\n"+"<"+getCurrentTimeAsString()+"> "+errorMessage;
    $("#logging textarea").val(logging_val);
    $("#logging textarea").scrollTop($("#logging textarea")[0].scrollHeight);
}
function reportOk(statusMessage) {
	$("#status").find(".infotext").html(statusMessage);
	$("#status").find(".ui-icon").removeClass("ui-icon-alert");
	$("#status").find(".ui-icon").removeClass("ui-icon-info");
	$("#status").find(".ui-icon").addClass("ui-icon-circle-check");
	$("#status").addClass("ui-state-highlight");
	$("#status").removeClass("ui-state-error");
    $(".ajax-loader").hide();
    logging_val = $("#logging textarea").val();
    logging_val += "\n"+"<"+getCurrentTimeAsString()+"> "+statusMessage;
    $("#logging textarea").val(logging_val);
    $("#logging textarea").scrollTop($("#logging textarea")[0].scrollHeight);
}
function reportStatus(statusMessage) {
	$("#status").find(".infotext").html(statusMessage);
	$("#status").find(".ui-icon").removeClass("ui-icon-alert");
	$("#status").find(".ui-icon").addClass("ui-icon-info");
	$("#status").find(".ui-icon").removeClass("ui-icon-circle-check");
	$("#status").addClass("ui-state-highlight");
	$("#status").removeClass("ui-state-error");
    $(".ajax-loader").hide();
    logging_val = $("#logging textarea").val();
    logging_val += "\n"+"<"+getCurrentTimeAsString()+"> "+statusMessage;
    $("#logging textarea").val(logging_val);
    $("#logging textarea").scrollTop($("#logging textarea")[0].scrollHeight);
}

activeCanvas = -1;
selectedWidget = -1;
selectedInput = -1;
selectedOutput = -1;
selectedConnection = -1;

connections = {};

executed = {};

function Connection(output,input) {
	this.output = output;
	this.input = input;

    this.inputWidget = $("#input"+input).parent().parent().attr('rel');
    this.outputWidget = $("#output"+output).parent().parent().attr('rel');

}

$(document).ajaxError(function(e, jqxhr, settings, exception) {

    if (jqxhr.status==504) {
        reportStatus("A widget is taking more than 15 minutes to run. Don't worry, it still works, but when it finishes you'll have to run the rest of the widgets manually or run the rest of the workflow again. We're soon fixing this bug, don't worry.");
    } else {

    reportError("An unexpected error has occured (possibly while executing a widget). We're sorry for this. A much nicer error message will be available soon! (The ClowdFlows team has been notified of this error)");

    }

});

$(document).ajaxSend(function(event, xhr, settings) {
    function getCookie(name) {
        var cookieValue = null;
        if (document.cookie && document.cookie != '') {
            var cookies = document.cookie.split(';');
            for (var i = 0; i < cookies.length; i++) {
                var cookie = jQuery.trim(cookies[i]);
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) == (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    }
    function sameOrigin(url) {
        // url could be relative or scheme relative or absolute
        var host = document.location.host; // host + port
        var protocol = document.location.protocol;
        var sr_origin = '//' + host;
        var origin = protocol + sr_origin;
        // Allow absolute or scheme relative URLs to same origin
        return (url == origin || url.slice(0, origin.length + 1) == origin + '/') ||
            (url == sr_origin || url.slice(0, sr_origin.length + 1) == sr_origin + '/') ||
            // or any other URL that isn't scheme relative or absolute i.e relative.
            !(/^(\/\/|http:|https:).*/.test(url));
    }
    function safeMethod(method) {
        return (/^(GET|HEAD|OPTIONS|TRACE)$/.test(method));
    }

    if (!safeMethod(settings.type) && sameOrigin(settings.url)) {
        xhr.setRequestHeader("X-CSRFToken", getCookie('csrftoken'));
    }
});

function synchronize(workflow_id) {
    $.post(url['synchronize-widgets'], {'workflow_id':workflow_id}, function(data) {
        $("#canvas"+workflow_id).append(data);

        updateWidgetListeners();
        resizeWidgets();

        $.post(url['synchronize-connections'], {'workflow_id':workflow_id}, function(data) {
            var connectionsArray = new Array();
             for (object in data) {
                if (data[object].model=="workflows.connection") {
                    connectionsArray.push(data[object]);
                }
            }
            for (c in connectionsArray) {
                connections[connectionsArray[c].pk] = new Connection(connectionsArray[c].fields.output,connectionsArray[c].fields.input);
            }

            redrawLines();

            setTimeout("redrawLines()",500);
        },'json')

    }
    ,'html')
}

function widgetExists(widget_id) {
    if ($("#widget"+widget_id).size()==1) {
        return true;
    } else return false;
}

function recursiveDelete(widget) {
    //alert($(widget).data("workflow_link"));
    var workflow_link = $(widget).data("workflow_link");
    $("#canvas"+workflow_link).find("div.widget").each(function() {
        if ($(widget).data("workflow_link")>0) {
            recursiveDelete($(this));
        }
    });

    $("#canvas"+workflow_link).remove();
    var i = 0;
    $("div#tabs ul li").each(function() {
        if ($(this).children("a").attr('href')=="#canvas"+workflow_link) {
            $("#tabs").tabs("remove",i);
        }
        i++;
    });

    for (c in connections) {
        if ($("#drawingcanvas"+c).size()==0) {
            delete connections[c];
        }
    }

}

function deleteSelected() {
	if (selectedWidget==-1&&selectedConnection==-1) {
		reportError("Nothing to delete.");
	} else if (selectedWidget!=-1) {
        var newSelected = selectedWidget;
        unfinish(selectedWidget);
        $.post(url['delete-widget'], { "widget_id": selectedWidget }, function(data) {
        if ($("#widget"+newSelected).data("workflow_link")>0) {
            recursiveDelete($("#widget"+newSelected));
        }
        $("#widget"+newSelected).find(".out_input").each(function() {
            //alert("yes");
            var currentId = $(this).attr('id');
            currentId = currentId.replace("output","");
            var deletedInput = $(".inner_output"+currentId);
            var deletedInputId = deletedInput.attr('id');
            deletedInputId = deletedInputId.replace("input","");
            var outer_widget_id = $("#widget"+newSelected).find(".outer-widget-link").attr('rel');
            var outer_widget_workflow_id = $("#widget"+newSelected).find(".outer-widget-workflow").attr('rel');
            deletedInput.remove();
            for (conId in connections) {
                if (connections[conId].input==deletedInputId) {
                    delete connections[conId];
                }
            }
            refreshWidget(outer_widget_id,outer_widget_workflow_id);

        });
        $("#widget"+newSelected).find(".out_output").each(function() {
            var currentId = $(this).attr('id');
            currentId = currentId.replace("input","");
            var deletedOutput = $(".inner_input"+currentId);
            var deletedOutputId = deletedOutput.attr('id');
            deletedOutputId = deletedOutputId.replace("output","");
            deletedOutput.remove();

            for (conId in connections) {
                if (connections[conId].output==deletedOutputId) {
                    delete connections[conId];
                }
            }

            var outer_widget_id = $("#widget"+newSelected).find(".outer-widget-link").attr('rel');
            var outer_widget_workflow_id = $("#widget"+newSelected).find(".outer-widget-workflow").attr('rel');


            refreshWidget(outer_widget_id,outer_widget_workflow_id);

        });
		$("#widget"+newSelected).remove();
        $("#widgetpreferences-"+newSelected).remove();
        selectedWidget=-1;
        for (conId in connections) {
            if (connections[conId].inputWidget==newSelected||connections[conId].outputWidget==newSelected) {
                $("#drawingcanvas"+conId).remove();
                $("#drawingoutline"+conId).remove();
                delete connections[conId];
            }
        }

        for (r in data.refresh) {
            var widget_id = data.refresh[r][0];
            var workflow_id = data.refresh[r][1];

            refreshWidget(widget_id,workflow_id);

        }

        if (data.delete_tab!=-1) {
            //$("#canvas"+data.delete_tab).remove()
                $("#canvas"+data.delete_tab).remove();
                var i = 0;
                $("div#tabs ul li").each(function() {
                    if ($(this).children("a").attr('href')=="#canvas"+data.delete_tab) {
                        $("#tabs").tabs("remove",i);
                    }
                    i++;
                });
        }


        },'json');
	} else if (selectedConnection!=-1) {
        var newSelected = selectedConnection;
        $.post(url['delete-connection'], { "connection_id": selectedConnection }, function(data) {
            selectedConnection=-1;
            unfinish(connections[newSelected].inputWidget);
            $("#drawingcanvas"+newSelected).remove();
            $("#drawingoutline"+newSelected).remove();
            delete connections[newSelected];
            if (data.refresh!=-1) {
            // refresh widget
                var old_Data = data;
                refreshWidget(data.refresh,data.refreshworkflow);
            }
        },'json');
	}
	//checkRequirements();
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


function showResults(widgetId) {

    $("#widgetresults-"+widgetId).dialog("destroy");
    $("#widgetresults-"+widgetId).remove();

    var dialog = $("#widgetresults-"+widgetId);
    var thisWidget = widgetId;

    $.post(url['widget-results'], {'widget_id':thisWidget}, function(data) {
        $("#dialogs").append(data);
        updateWidgetListeners();
        $("#widgetresults-"+thisWidget).dialog('open');
    },'html');
}

function copyWidget(widgetId) {
        $(".ajax-loader").show();
        $.post(url['add-widget'], { 'copywidget_id' : widgetId, 'active_workflow' : activeCanvasId, 'scrollTop':	activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft() }, function(data) {
            $(".ajax-loader").hide();
            activeCanvas.append(data);
            updateWidgetListeners();
            resizeWidgets();
        },'html');

}

function showDocumentation(widgetId) {

    $("#widgetdocumentation-"+widgetId).dialog("destroy");
    $("#widgetdocumentation-"+widgetId).remove();

    var dialog = $("#widgetdocumentation-"+widgetId);
    var thisWidget = widgetId;

    $.post(url['documentation'], {'widget_id':thisWidget}, function(data) {
        $("#dialogs").append(data);
        updateWidgetListeners();
        $("#widgetdocumentation-"+thisWidget).dialog('open');
    },'html');
}

function visualizeWidget(widgetId) {

    $("#widgetvisualization-"+widgetId).dialog("destroy");
    $("#widgetvisualization-"+widgetId).remove();

    var dialog = $("#widgetvisualization-"+widgetId);
    var thisWidget = widgetId;

    $.post(url['widget-visualization'], {'widget_id':thisWidget}, function(data) {
        $("#dialogs").append(data);
        updateWidgetListeners();
        $("#widgetvisualization-"+thisWidget).dialog('open');
    },'html');
}

function unfinish(widgetId) {
    $(".statusimage"+widgetId).hide();
    $(".widget"+widgetId+"progressbar").css('width','0px');
    $(".widget"+widgetId+"progress").hide();
    executed[widgetId]=false;
    for (c in connections) {
        if (connections[c].outputWidget==widgetId) {
            unfinish(connections[c].inputWidget);
        }
    }
}

function unfinishOne(widgetId) {
    $(".statusimage"+widgetId).hide();
    $(".widget"+widgetId+"progressbar").css('width','0px');
    $(".widget"+widgetId+"progress").hide();
    executed[widgetId]=false;
}
function unfinishDescendants(widgetId) {
    for (c in connections) {
        if (connections[c].outputWidget==widgetId) {
            unfinish(connections[c].inputWidget);
        }
    }
}

function runWidget(widgetId) {

    $(".statusimage"+widgetId).hide();
    $(".running"+widgetId).show();
    if ($("#widget"+widgetId).hasClass("has_progress_bar")) {
        $(".widget"+widgetId+"progress").show();
        $(".widget"+widgetId+"progressbar").css('width','0px');
        setTimeout("updateProgressBar("+widgetId+")",1000);
    }
    $.post(url['run-widget'], { 'widget_id':widgetId }, function(data) {

       if (data.status=="ok") {
        unfinishDescendants(widgetId);
        reportOk(data.message);
       $(".statusimage"+widgetId).hide();
       $(".done"+widgetId).show();
       } else if (data.status=="error") {
        reportError(data.message);
        $(".statusimage"+widgetId).hide();
        $(".error"+widgetId).show();
       } else if (data.status=="interactive") {
        unfinishDescendants(widgetId);
        reportOk(data.message);
        displayInteraction(data.widget_id)
       } else if (data.status=="visualize") {
                    unfinishDescendants(data.widget_id);
                    visualizeWidget(data.widget_id);
                    reportOk(data.message);
                   $(".statusimage"+data.widget_id).hide();
                   $(".done"+data.widget_id).show();
       }
       executed[widgetId] = true;

    },'json');
}

function waitPredecessorAndRunWidget(widgetId) {
    allRun = true;
    for (c in connections) {
        if (connections[c].inputWidget==widgetId) {
            if(!executed[connections[c].outputWidget]) {
                allRun = false;
            }
        }
    }
    if (allRun)
        runWidget(widgetId);
    else
        setTimeout(function(){waitPredecessorAndRunWidget(widgetId);},100);
}

function runTree(widgetId) {
    $.post(url['get-executed-status'], { 'workflow_id':activeCanvasId }, function(data) {
        executed = data.executedStatus;
        if(executed[widgetId])
        //first reset widget and then run tree
            $.post(url['reset-widget'], { 'widget_id':widgetId }, function(data) {
                unfinish(widgetId)
                runTreeRec(widgetId);
            },'json');
        else
        //dont need to reset widget, just run tree
            runTreeRec(widgetId);
    },'json');
}

function runTreeRec(widgetId) {
    if(executed[widgetId]) return;

    for (c in connections) {
        if (connections[c].inputWidget==widgetId) {
            runTreeRec(connections[c].outputWidget);
        }
    }
    waitPredecessorAndRunWidget(widgetId);
}

function resetWidget(widgetId) {
    $.post(url['reset-widget'], { 'widget_id':widgetId }, function(data) {
        unfinish(widgetId)
        /*for (c in connections) {
            if (connections[c].outputWidget==widgetId) {
                resetWidget(connections[c].inputWidget);
            }
        }*/
    },'json');
}

function resetWorkflow() {
    $.post(url['reset-workflow'], { 'workflow_id':activeCanvasId }, function(data) {
        for (i in data.resetWidget) {
            unfinishOne(data.resetWidget[i]);
        }
    },'json');
}

function updateProgressBar(widgetId) {
     $.get(url['widget-progress'], { 'widget_id':widgetId }, function(data) {
        if (data!="-1") {
            $(".widget"+widgetId+"progressbar").css('width',data+'%');
            if (data!="100") {
                setTimeout("updateProgressBar("+widgetId+")",1000);
            } else {
                unfinishDescendants(widgetId);
                $(".statusimage"+widgetId).hide()
                $(".done"+widgetId).show()
            }
        } else {
            $(".widget"+widgetId+"progress").hide();
        }
    });
}

function displayInteraction(widgetId) {
    $("#widgetinteract-"+widgetId).dialog("destroy");
    $("#widgetinteract-"+widgetId).remove();

    var thisWidget = widgetId;

    $.post(url['widget-interaction'], {'widget_id':thisWidget}, function(data) {
        $("#dialogs").append(data);
        updateWidgetListeners();
        $("#widgetinteract-"+thisWidget).dialog('open');
    },'html');
}

function runWorkflowWidget(widgetId,workflowId) {

    //cekiri ce je finished
    if (!executed[widgetId]) {

        $(".statusimage"+widgetId).hide();
        $(".running"+widgetId).show();

        if ($("#widget"+widgetId).hasClass("has_progress_bar")) {
            $(".widget"+widgetId+"progress").show();
            $(".widget"+widgetId+"progressbar").css('width','0px');
            setTimeout("updateProgressBar("+widgetId+")",1000);
        }

        $.post(url['run-widget'], { 'widget_id':widgetId }, function(data) {

               $(".statusimage"+widgetId).hide();
               $(".done"+widgetId).show();

           if (data.status=="ok") {
            reportOk(data.message);
            runWorkflow(workflowId);
           } else if (data.status=="error") {
            reportError(data.message);
                $(".statusimage"+widgetId).hide();
        $(".error"+widgetId).show();
        } else if (data.status=="interactive") {
        unfinishDescendants(widgetId);
        reportOk(data.message);
        displayInteraction(data.widget_id)
           } else if (data.status=="visualize") {
                    unfinishDescendants(data.widget_id);
                    visualizeWidget(data.widget_id);
                    reportOk(data.message);
                   $(".statusimage"+data.widget_id).hide();
                   $(".done"+data.widget_id).show();
       }

        },'json');
        executed[widgetId]=true;
    }
}

function runWorkflow(workflowId) {


    $.post(url['get-unfinished'], {'workflow_id':workflowId}, function(data) {
        for (widgetIndex in data.ready_to_run) {
            widgetId = data.ready_to_run[widgetIndex];
            //setTimeout('runWidget('+widgetId+')',10);
            //executed.append(data.ready_to_run[widgetIndex]);
            runWorkflowWidget(widgetId,workflowId);
        }
    },'json')
}
/* works in a similar way to updateConnectionListeners(), it should be called when a new widget has been drawn on the canvas.
Each widget may be dragged along the canvas. While the users is dragging the widget the lines are redrawn in real time (unless direct lines are used)
Clicking on a widget selects it. Clicking on an input of a widget selects the widget and the input. Clicking on an output of the widget selects the widget and the output.
When clicking on an input a check is made to determine whether an output is already selected. If yes, a connection is attempted.
In the same way when clicking on an output a check is made to determine whether an input is already selected. If selected, a connection is attempted.
*/
function updateWidgetListeners() {

    $(".widgetnameinput").unbind("change");
    $(".widgetnameinput").change(function() {
        /*w = $(this).parent().parent().attr('rel');
        activeWidgets[w].name = $(this).val();
        $("#widgetcaption"+w).html($(this).val());
        $("span[rel=#canvas"+w+"]").html($(this).val());*/
    });


    $("#dialogs div.widgetdialog").dialog({
    autoOpen: false,
    modal: false,
    resizable: true,
    buttons: {
            "Apply": function() {
                changed = false;
                $(this).find("input").each(function() {
                    if ($(this).attr('type')!="file"&&$(this).attr('type')!="hidden") {
                        var paramId = $(this).attr('id').replace("pref-","")
                        var paramVal = $(this).val();
                        if ($(this).attr('type')=="checkbox") {
                            if (!$(this).is(":checked")) {
                                paramVal = ''
                            }
                        }
                        $.post(url['save-parameter'], { 'input_id':paramId, 'value':paramVal });
                        changed = true;
                        $(".statusimage"+widgetId).hide();
                    }
                });
                $(this).find("textarea").each(function() {
                        var paramId = $(this).attr('id').replace("pref-","")
                        var paramVal = $(this).val();
                        $.post(url['save-parameter'], { 'input_id':paramId, 'value':paramVal });
                        changed = true;
                        $(".statusimage"+widgetId).hide();
                });
                $(this).find("select").each(function() {
                    var paramId = $(this).attr('id').replace("pref-","")
                    var paramVal = $(this).val();
                    $.post(url['save-parameter'], { 'input_id':paramId, 'value':paramVal });
                    changed = true;
                    $(".statusimage"+widgetId).hide();
                });
                var widgetId = $(this).attr('id').replace("widgetpreferences-","");
                if (changed) {
                    unfinish(widgetId);
                }
                $(this).dialog("close");
                $(this).remove();
            }
            ,
            "Close": function() {
                $(this).dialog("close");
                $(this).remove();
            }
        }
    });

    $("#dialogs div.widgetconfdialog").dialog({
        autoOpen: false,
        modal: false,
        resizable: true,
        width: 600,
        buttons: {
            "Apply": function() {
                var inputs = new Array();
                $(this).find("#inputs").children().each(function() {
                    var id = $(this).attr('id').replace("input-","");
                    inputs.push(parseInt(id));
                });
                var params = new Array();
                $(this).find("#params").children().each(function() {
                    var id = $(this).attr('id').replace("input-","");
                    params.push(parseInt(id));
                });
                var outputs = new Array();
                $(this).find("#outputs").children().each(function() {
                    var id = $(this).attr('id').replace("output-","");
                    outputs.push(parseInt(id));
                });
                var widgetId = $(this).attr('id').replace("widgetconfiguration-","");

                var benchmark = $("#benchmark-"+widgetId)[0].checked;

                $.ajax({
                    url: url['save-configuration'],
                    type: "POST",
                    data: { 'widgetId':widgetId, 'inputs':inputs, 'params':params, 'outputs':outputs, 'benchmark':benchmark },
                    dataType: "json",
                    traditional: true,
                    success: function(data) {
                        if (data.changed || data.reordered) {
                            unfinish(widgetId);
                            refreshWidget(widgetId, activeCanvasId);
                            for (var i=0; i< data.deletedConnections.length; i++) {
                                var conId = data.deletedConnections[i];
                                $("#drawingcanvas"+conId).remove();
                                $("#drawingoutline"+conId).remove();
                                delete connections[conId];
                            }
                            $('#widgetpreferences-'+widgetId).remove();
                            $('#widgetconfiguration-'+widgetId).remove();
                            reportStatus("Successfully saved widget configuration.");
                        }
                        else {
                            $('#widgetconfiguration-'+widgetId).remove();
                        }
                    },
                    error: function(e,f) {
                        $('#widgetpreferences-'+widgetId).remove();
                        $('#widgetconfiguration-'+widgetId).remove();
                        reportError("Error saving widget configuration!");
                    }
                });
            }
            ,
            "Close": function() {
                $(this).dialog("close");
                $(this).remove();
            }
        }
    });

    $("#dialogs div.widgetrenamedialog").dialog({
    autoOpen: false,
    modal: false,
    resizable: true,
    buttons: {
            "Apply": function() {

                var newName = $(this).find(".widgetnameinput").val()
                var widgetId = ($(this).attr('rel'));
                $.post(url['rename-widget'], { 'new_name':newName, 'widget_id':widgetId }, function(data) {

                $("#widgetcaptionspan"+widgetId).html(newName);

                if (data.workflow_link==true) {
                    $("span[rel=#canvas"+data.workflow_link_id+"]").html(newName);
                }

                for (i in data.rename_inputs) {

                    $("#input"+data.rename_inputs[i]).html(newName.substring(0,3));

                }

                for (i in data.rename_outputs) {
                    $("#output"+data.rename_outputs[i]).html(newName.substring(0,3));
                }

                },'json');
                $(this).dialog("close");
            }
            ,
            "Close": function() {
                $(this).dialog("close");
            }
        }
    });

    $("#dialogs div.widgetdesignationdialog").dialog({
    autoOpen: true,
    modal: false,
    resizable: true,
    buttons: {
            "Apply": function() {

                var inputDesignation = {};

                $(this).find("input").each(function() {

                    if (($(this).attr('type')=="radio")&&($(this).is(":checked"))) {
                        var paramId = $(this).attr('name').replace("inputdesignation-","")
                        var paramVal = $(this).val();
                        inputDesignation[paramId]=paramVal;
                        //$.post(url['save-parameter'], { 'input_id':paramId, 'value':paramVal });
                        //changed = true;
                        //$(".statusimage"+widgetId).hide();
                    }




                });

                $.post(url['save-designation'], inputDesignation);

                $(this).dialog("close");
                $(this).dialog("destroy");
                $(this).remove();
            }
            ,
            "Close": function() {
                $(this).dialog("close");
                $(this).dialog("destroy");
                $(this).remove();
            }
        }
    });

    $("#dialogs div.widgetresultsdialog").dialog({
    autoOpen: false,
    width: 500,
    modal: false,
    resizable: true,
    buttons: {
            "Close": function() {
                $(this).dialog("close");
                $(this).dialog("destroy");
                $(this).remove();
            }
        }
    });

    $("#dialogs div.widgetvisualizationdialog").each(function() {

    var thisWidth = 500
    var thisHeight = 400

    if (parseInt($(this).attr('width'))>0) {
        thisWidth = parseInt($(this).attr('width'));
    }

    if (parseInt($(this).attr('height'))>0) {
        thisHeight = parseInt($(this).attr('height'))
    }

    $(this).dialog({
    autoOpen: false,
    width: thisWidth,
    height: thisHeight,
    modal: false,
    resizable: true,
    buttons: {
            "Close": function() {
                $(this).dialog("close");
                $(this).dialog("destroy");
                $(this).remove();
            }
        }
    });

    });

    $("#dialogs div.widgetdocumentationdialog").each(function() {

    var thisWidth = 500
    var thisHeight = 400

    if (parseInt($(this).attr('width'))>0) {
        thisWidth = parseInt($(this).attr('width'));
    }

    if (parseInt($(this).attr('height'))>0) {
        thisHeight = parseInt($(this).attr('height'))
    }

    $(this).dialog({
    autoOpen: false,
    width: thisWidth,
    height: thisHeight,
    modal: false,
    resizable: true,
    buttons: {
            "Close": function() {
                $(this).dialog("close");
                $(this).dialog("destroy");
                $(this).remove();
            }
        }
    });

    });

    $("#dialogs div.widgetinteractdialog").each(function() {

    var thisWidth = 500
    var thisHeight = 400

    if (parseInt($(this).attr('width'))>0) {
        thisWidth = parseInt($(this).attr('width'));
    }

    if (parseInt($(this).attr('height'))>0) {
        thisHeight = parseInt($(this).attr('height'))
    }

    $(this).dialog({
    autoOpen: false,
    width: thisWidth,
    height: thisHeight,
    modal: false,
    resizable: true,

    buttons: {
            "Apply": function() {
                var form = $(this).find("form");
                if (form.find(".runfunction").val()!=undefined) {
                    eval(form.find(".runfunction").val());
                }
                var serialized = form.serialize();
                $.post(url['finish-interaction'], serialized, function(data) {
                   if (data.status=="ok") {
                    unfinishDescendants(data.widget_id);
                    reportOk(data.message);
                   $(".statusimage"+data.widget_id).hide();
                   $(".done"+data.widget_id).show();
                   } else if (data.status=="error") {
                    reportError(data.message);
                    $(".statusimage"+data.widget_id).hide();
                    $(".error"+data.widget_id).show();
                   } else if (data.status=="visualize") {
                    unfinishDescendants(data.widget_id);
                    visualizeWidget(data.widget_id);
                    reportOk(data.message);
                   $(".statusimage"+data.widget_id).hide();
                   $(".done"+data.widget_id).show();
                   }

                   $("#widgetinteract-"+data.widget_id).dialog("destroy");
                   $("#widgetinteract-"+data.widget_id).remove();

                },'json');

                $(this).dialog("close");
                $(this).dialog("destroy");
                $(this).remove();
            }
        }
    });

    });

    $(".canvas div.widget").unbind("click");
    $(".canvas div.widget").unbind("dblclick");
    $(".canvas div.widget div.input").unbind("click");
    $(".canvas div.widget div.output").unbind("click");

		$(".canvas div.widget").each(function() {

			var thisWidgetId = $(this).attr('rel');
			//alert(thisWidgetId);

			if ($(this).data('contextMenu')!=true) {
				$(this).contextMenu({
					menu: 'widgetMenu'
				},
					function(action, el, pos) {


					if (action=='delete') {
                        selectedWidget = $(el).attr('rel');
                        selectedConnection=-1;
						deleteSelected();
					}

					if (action=='run') {
                        runWidget(thisWidgetId);

					}

					if (action=='properties') {
                        $("#widget"+thisWidgetId).dblclick();
					}

                    if (action=='runtree') {
                        runTree(thisWidgetId);

                    }

                    if (action=='resetwidget') {
                        resetWidget(thisWidgetId);
                    }

                    if (action=='resetworkflow') {
                        resetWorkflow();
                    }

					if (action=='results') {
						showResults(thisWidgetId);
					}

					if (action=='rename') {
                        var dialog = $("#widgetrename-"+$(el).attr('rel'));
                        var thisWidget = $(el).attr('rel');

                        if (dialog.size()==0) {
                            $.post(url['get-rename'], {'widget_id':$(el).attr('rel')}, function(data) {
                                $("#dialogs").append(data);
                                updateWidgetListeners();
                                $("#widgetrename-"+thisWidget).dialog('open');
                            },'html');
                        } else {
                            dialog.dialog('open');
                        }
					}

                    if (action=='copy') {
                        copyWidget(thisWidgetId);
                    }

                    if (action=='help') {
                        showDocumentation(thisWidgetId);
                    }


				});
				$(this).data('contextMenu',true);
			}
		});

    $(".canvas div.widget").dblclick(function(){
        if ($(this).hasClass("subprocess")) {

            var this_workflow_link = $(this).find(".workflow_link").attr('rel');
            if ($("#canvas"+this_workflow_link).size()==0) {
                var thisWidget = this;
                $.post(url['get-subprocess'], { 'widget_id':$(thisWidget).attr('rel') }, function(data) {
                $(thisWidget).data("workflow_link",data.workflow_link);

                $("#tabs").append('<div rel="'+data.workflow_link+'" class="canvas'+data.workflow_link+' canvas" id="canvas'+data.workflow_link+'"><svg xmlns="http://www.w3.org/2000/svg" version="1.1" style="position:absolute;top:0px;left:0px;width:100%;height:100%;"></svg></div>');
                $("#tabs").tabs("add","#canvas"+data.workflow_link,data.workflow_name);
                $("#tabs").tabs("select","#canvas"+$(thisWidget).data('workflow_link'));
                activeCanvasId = $(thisWidget).data('workflow_link');
                activeCanvas = $(".canvas"+activeCanvasId);
                resizeCanvas();

                synchronize($(thisWidget).data("workflow_link"));

            },'json');

            } else {
            $("#tabs").tabs("select","#canvas"+this_workflow_link);
            activeCanvasId = this_workflow_link;
            activeCanvas = $(".canvas"+activeCanvasId);
            resizeCanvas();

            }
        } else {
        //$("#widgetpreferences-"+$(this).attr('rel')).dialog('open');

            var dialog = $("#widgetpreferences-"+$(this).attr('rel'));
            var thisWidget = $(this).attr('rel');

            if (dialog.size()==0) {
                $.post(url['get-parameters'], {'widget_id':$(this).attr('rel')}, function(data) {
                    $("#dialogs").append(data);
                    updateWidgetListeners();
                    fileListeners();
                    $("#widgetpreferences-"+thisWidget).dialog('open');
                },'html');
            } else {
                dialog.dialog('open');
            }
        }
    });

    $(".canvas div.widget").draggable({
        multiple: true,
        handle: "div.widgetcenter, img.widgetimage",
        drag: function() {
            // this function exectues every time the mouse moves and the user is holding down the left mouse button
            for (c in connections) {

                if (connections[c].inputWidget==$(this).attr('rel')) {
                  //  $(".connection"+c).remove();
                    drawConnection(c);
                }

                if (connections[c].outputWidget==$(this).attr('rel')) {

                  //  $(".connection"+c).remove();
                    drawConnection(c);
                }

            }



            resizeSvg();

            /*var y = parseInt($(this).css('top'));
            var x = parseInt($(this).css('left'));*/



        },

        stop: function() {
            // this function exectues when the user stops dragging the widget
                if (($(this).css('left')).charAt(0)=='-') {
                    $(this).css('left','0px');
                }
                if (($(this).css('top')).charAt(0)=='-') {
                    $(this).css('top','0px');
                }


                //alert($(this).attr('rel'));

               //get all selected widgets and save positions
               $(".ui-selected").each(function () {
                var y = parseInt($(this).css('top'));
                var x = parseInt($(this).css('left'));
                $.post(url['save-position'], { "widget_id": $(this).attr('rel'), "x": x, "y": y } );

                
               })


               if ($(".ui-selected").size()==0) {
                var y = parseInt($(this).css('top'));
                var x = parseInt($(this).css('left'));
                $.post(url['save-position'], { "widget_id": $(this).attr('rel'), "x": x, "y": y } );
               }
               
                redrawLines();

        }}
    );

    $(".canvas div.widget").click(function(e) {
        selectedWidget = $(this).attr('rel');
        if (!(e.ctrlKey || e.metaKey || e.shiftKey))
        {
            $(".widgetcenter").removeClass("ui-state-highlight");
            $(".widget").removeClass("ui-selected");
        } else {
            if ($(this).hasClass("ui-selected")) {
                $(this).removeClass("ui-selected");
                $(this).find(".widgetcenter").removeClass("ui-state-highlight");
                return;
            }
        }
        $(this).find(".widgetcenter").addClass("ui-state-highlight");
        $(this).addClass("ui-selected");
        selectedConnection=-1;

        //clicking on a widget selects it and deselects any connection

        redrawLines();

    });



		$(".canvas div.widget div.input").click(function() {
			selectedInput = parseInt($(this).attr('id').replace("input",""));

			$(".input").removeClass("ui-state-highlight");
			$(this).addClass("ui-state-highlight");

			if (selectedOutput!=-1) {
				addConnection(selectedOutput,selectedInput);

			}
		});

		$(".canvas div.widget div.output").click(function() {
			selectedOutput = parseInt($(this).attr('id').replace("output",""));

			$(".output").removeClass("ui-state-highlight");
			$(this).addClass("ui-state-highlight");
			if (selectedInput!=-1) {
				addConnection(selectedOutput,selectedInput);
			}
		});

        $(".interactionwaiting").each(function() {
            $(this).removeClass("interactionwaiting");
            var widgetId = $(this).attr('rel');
            displayInteraction(widgetId);
        });

}

function openConfiguration(thisWidgetId) {
    var dialog = $("#widgetconfiguration-"+thisWidgetId);
    if (dialog.size()==0) {
        $.post(url['get-configuration'], {'widget_id':thisWidgetId}, function(data) {
            $("#dialogs").append(data);
            updateWidgetListeners();
            fileListeners();
            dialog = $("#widgetconfiguration-"+thisWidgetId);
            $("#params").sortable({connectWith:".inputsParams", placeholder:"ui-state-highlight"}).disableSelection();
            $("#inputs").sortable({connectWith:".inputsParams", placeholder:"ui-state-highlight"}).disableSelection();
            $("#outputs").sortable({placeholder:"ui-state-highlight"}).disableSelection();
            dialog.dialog('open');
        },'html');
    } else {
        dialog.dialog('open');
    }
}

function addConnection(output,input) {

    $.post(url['add-connection'], { "output_id": output, "input_id": input }, function(data) {
        if (data.success==true) {
            reportOk(data.message);
            if (data.deleted!=-1) {
                //remove connection
                delete connections[data.deleted];
                $(".connection"+data.deleted).remove();
            }
            if (data.added!=-1) {
                //add connection

                connections[data.added] = new Connection(data.output_id,data.input_id);
                drawConnection(data.added);
                unfinish(connections[data.added].inputWidget);

                selectedInput=-1;
				selectedOutput=-1;
				$(".input").removeClass("ui-state-highlight");
				$(".output").removeClass("ui-state-highlight");

            }
            if (data.refresh!=-1) {
            // refresh widget
                var old_Data = data;
                refreshWidget(data.refresh,data.refreshworkflow);
            }
        } else {
            reportError(data.message);
            selectedInput=-1;
            selectedOutput=-1;
            $(".input").removeClass("ui-state-highlight");
            $(".output").removeClass("ui-state-highlight");
        }
    },'json' );

}


/* resizeWidgets() selects all currently active widgets, counts their inputs and outputs and resizes them accordingly,
so that graphically they appear longer when there are larger number of inputs/outputs used. Each excess input/output adds 29pixels
to the height of the widget. The icon representing the widget is then vertically aligned to the middle of the widget */
function resizeWidgets() {
	$("div.canvas div.widget").each(function () {

		var widget = $(this).children("div.widgetcenter").height();
		var inputs = $(this).children("div.inputs").height();
		var outputs = $(this).children("div.outputs").height();

        max = widget;

        if (inputs>max) {
            max=inputs;
        }

        if (outputs>max) {
            max=outputs;
        }

        $(this).children("div.widgetcenter").css('height',max+'px');

    });
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
		setTimeout("redrawLines()",100);
        setTimeout("resizeSvg();",100);
	},
	tabTemplate: '<li><a href="#{href}"><span rel="#{href}">#{label}</span></a></li>'
	});

	activeCanvas = $(".canvas").eq(0);

	resizeCanvas();

	$(window).bind('resize', function() {

	resizeCanvas();

	});
	//jg = new jsGraphics("canvas0");
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

    $(".run").click(function() {
        $.post(url['unfinish-vizualizations'], { 'workflow_id':activeCanvasId }, function(data) {
        for (i in data.unfinished) {
            unfinish(data.unfinished[i]);
        }
        runWorkflow(activeCanvasId);
        },'json');
    });

	$(window).keydown(function(event) {
		if (event.keyCode==46) {
            var dialog_open = false,
                search_focus = $('#searchBox').is(':focus');
            $(".ui-dialog").each(function() {
                if ($(this).is(":visible")) {
                    dialog_open = true;
                }
            });
            if (!dialog_open && !search_focus) {
                deleteSelected();
            }
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

	$(".save").click(function() {
		$("#savedialog").dialog('open');
	});

	$(".loadwidget").click(function() {
		$("#loadwidgetdialog").dialog('open');
	});

	$(".info").click(function() {
        $.get(url['workflow-url'], function(data) {
        reportStatus(data)
    },'html');

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
			    top.location.href = url['new-workflow'];
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
                    $(".ajax-loader").show();
					$.ajax({
						url: url['import-webservice'],
						type: "POST",
						data: postdata,
						dataType: "json",
						success: function(data) {
							$(this).dialog("close");
                            $("#wsdlinput").val("");
                            $(".ajax-loader").hide();
                            addCategory(data.category_id);
                            designateInputs(data.category_id);


						},
						error: function(e,f) {
							reportError("Error: Cannot import webservice."+f);
                            $(".ajax-loader").hide();
							$(this).dialog("close");
						}
					});

			},
			"Cancel": function() {
				$(this).dialog("close");
			}
		}
	});

	/*$('#rundialog').dialog({
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
	});*/

	$('#opendialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
			"Close": function() {
				$(this).dialog("close");
			}
		}
	});

	$('#savedialog').dialog({
		autoOpen: false,
		modal: true,
		resizable: false,
		buttons: {
            "Apply": function() {
                var newName = $(this).find(".workflownameinput").val();
                var description = $(this).find(".workflowdescriptioninput").val();
                var pub = $(this).find(".workflowpublicinput").val();
                if (!$(this).find(".workflowpublicinput").is(":checked")) {
                    pub = 'false'
                }
                var workflowId = ($(this).attr('rel'));
                $.post(url['rename-workflow'], { 'new_name':newName, 'workflow_id':workflowId, 'description':description, 'public':pub }, function(data) {

                    $("span[rel=#canvas"+data.workflow_id+"]").html(newName);

                },'json');

                $(this).dialog("close");

            },
			"Close": function() {
				$(this).dialog("close");
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


    $(".browser").treeview({
        persist: "cookie",
		cookieId: "workflowstree"}
    );

    /*$("#demo1").jstree();

    $("#selector").jstree({
			// the `plugins` array allows you to configure the active plugins on this instance
			"plugins" : ["themes","html_data","ui"],
		});*/



    $("#widgets a.subprocess").click(function() {
        $.post(url['add-subprocess'], {'active_workflow' : activeCanvasId, 'scrollTop':	activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft()}, function(data) {
            activeCanvas.append(data);
            updateWidgetListeners();
            resizeWidgets();
        },'html' );
    });

    $("#widgets a.forloop").click(function() {
         $.post(url['add-for'], {'active_workflow' : activeCanvasId, 'scrollTop':	activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft()}, function(data) {
            try {
                jsonData = $.parseJSON(data)
                if (jsonData.success==false) {
                    reportError(jsonData.message)
                }
            }
            catch (err)
            {
                activeCanvas.append(data);
                var outer_widget_id = $(data).find(".outer-widget-link").attr('rel');
                var outer_widget_workflow_id = $(data).find(".outer-widget-workflow").attr('rel');
                $("#widget"+outer_widget_id).remove();
                refreshWidget(outer_widget_id,outer_widget_workflow_id);
                updateWidgetListeners();
                resizeWidgets();
            }
        },'html');

    });

    $("#widgets a.crossvalidation").click(function() {
         $.post(url['add-cv'], {'active_workflow' : activeCanvasId, 'scrollTop':   activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft()}, function(data) {
            try {
                jsonData = $.parseJSON(data)
                if (jsonData.success==false) {
                    reportError(jsonData.message)
                }
            }
            catch (err)
            {
                activeCanvas.append(data);
                var outer_widget_id = $(data).find(".outer-widget-link").attr('rel');
                var outer_widget_workflow_id = $(data).find(".outer-widget-workflow").attr('rel');
                $("#widget"+outer_widget_id).remove();
                refreshWidget(outer_widget_id,outer_widget_workflow_id);
                updateWidgetListeners();
                resizeWidgets();
            }
        },'html');

    });

    $("#widgets a.input").click(function() {
         $.post(url['add-input'], {'active_workflow' : activeCanvasId, 'scrollTop':	activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft()}, function(data) {
            try {
                jsonData = $.parseJSON(data)
                if (jsonData.success==false) {
                    reportError(jsonData.message)
                }
            }
            catch (err)
            {
                activeCanvas.append(data);
                var outer_widget_id = $(data).find(".outer-widget-link").attr('rel');
                var outer_widget_workflow_id = $(data).find(".outer-widget-workflow").attr('rel');
                $("#widget"+outer_widget_id).remove();
                refreshWidget(outer_widget_id,outer_widget_workflow_id);
                updateWidgetListeners();
                resizeWidgets();
            }
        },'html');

    });

    $("#widgets a.output").click(function() {
          $.post(url['add-output'], {'active_workflow' : activeCanvasId, 'scrollTop':	activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft()}, function(data) {
            try {
                jsonData = $.parseJSON(data)
                if (jsonData.success==false) {
                    reportError(jsonData.message)
                }
            }
            catch (err)
            {
                activeCanvas.append(data);
                var outer_widget_id = $(data).find(".outer-widget-link").attr('rel');
                var outer_widget_workflow_id = $(data).find(".outer-widget-workflow").attr('rel');
                $("#widget"+outer_widget_id).remove();
                refreshWidget(outer_widget_id,outer_widget_workflow_id);
                updateWidgetListeners();
                resizeWidgets();
            }
        },'html');
    });

	resizeWidgets();

	$(".importWebservice").button();
	$(".importWebservice").click(function() {
		$("#wsdldialog").dialog('open');
	});


    refreshAddWidgetListeners();

    synchronize(activeCanvasId);

    setTimeout("refreshProgressBars()",5000);

    resizeSvg();

    setTimeout("resizeSvg()",1000);

    //MatjazJ: Make links for admin editing widgets and categories directly from treeview
    //doesnt matter if user manually tammpers this setting as he will not have permission to enter admin mode due to the provided django security
    if(typeof userIsStaff === 'undefined') userIsStaff=false;
    if(userIsStaff){
        $(".wid, .folder").each(function () {

            var thisWidgetType = $(this).attr('relType');
            var thisWidgetId = $(this).attr('rel');


            if ($(this).data('contextMenu') != true) {
                $(this).contextMenu({
                        menu:'widMenu'
                    },
                    function (action, el, pos) {

                        if (action == 'edit') {
                            window.open('/admin/workflows/'+thisWidgetType+'/'+thisWidgetId, '', '');
                        }
                    });
                $(this).data('contextMenu', true);
            }
        });
    }
});

function refreshProgressBars() {
    $(".currentlyrunning").each(function() {

        if ($(this).hasClass("has_progress_bar")) {
            $(".widget"+$(this).attr('rel')+"progress").show();
            updateProgressBar($(this).attr('rel'));
        }

    });
}

function refreshAddWidgetListeners() {

    $("#widgets a.widget").unbind("click");
    $("#widgets a.widget").click(function() { // this happens every time a new widget is put onto the canvas

        $.post(url['add-widget'], { 'abstractwidget_id' : $(this).attr('rel'), 'active_workflow' : activeCanvasId, 'scrollTop':	activeCanvas.scrollTop(), 'scrollLeft':activeCanvas.scrollLeft() }, function(data) {

            activeCanvas.append(data);
            updateWidgetListeners();
            resizeWidgets();
        },'html');

    });
}

function addCategory(category_id) {
    //alert(category_id)
    //refresh list

        $.post(url['get-category'], {'category_id':category_id}, function(data) {

        var branches = $(data).appendTo("#userwidgets");
        $(".browser").treeview({
            add: branches
        });

        refreshAddWidgetListeners();

        }
        ,'html');



}

function designateInputs(category_id) {

        $.post(url['get-designate-dialogs'], {'category_id':category_id}, function(data) {
            $("#dialogs").append(data);
            updateWidgetListeners();
        }
        ,'html');



}

function refreshWidget(widget_id,workflow_id) {

    $.post(url['get-widget'], {'widget_id':widget_id}, function(data) {
        $("#widget"+widget_id).remove();
        $("#canvas"+workflow_id).append(data);
        updateWidgetListeners();
        resizeWidgets();
        redrawLines();

    },'html');
}

function drawConnection(connectionid,bgcolor,color) {

    var conn = connections[connectionid];
    if ($("#input"+conn.input).parent().parent().parent().attr('rel')!=activeCanvas.attr('rel')) {
        return;
    }


	if (bgcolor==undefined) {
		//bgcolor='#0000ff';
        bgcolor='#a3a3a3';
	}
	if (color==undefined) {
		//color='rgb(173, 216, 230)';
        color='#d1d1d1';
	}

	if (selectedConnection==connectionid) {
		bgcolor='#ff0000';
		color='#ffaaaa';
	}

	var canvasPos = activeCanvas.offset();

    if (canvasPos != null) {
	var connection = connections[connectionid];

	input = $("#input"+connection.input);
	output = $("#output"+connection.output);

    //alert("#output"+connection.output);

	inputPos = input.position();
	outputPos = output.position();
    inputParent = input.parent().parent();
    outputParent = output.parent().parent();

	/*
	outputX = outputPos.left-canvasPos.left+(output.width());
	outputY = outputPos.top-canvasPos.top+(output.height()/2);
	inputX = inputPos.left-canvasPos.left;
	inputY = inputPos.top-canvasPos.top+(input.height()/2);
	*/
    outputX = outputPos.left+parseInt(outputParent.css('left'))+50;
    outputY = outputPos.top+parseInt(outputParent.css('top'))+30;
    inputX = inputPos.left+parseInt(inputParent.css('left'))+10;
    inputY = inputPos.top+parseInt(inputParent.css('top'))+30;
	drawingConnection = connectionid;

      var p1 = [outputX,outputY];
      var p2 = [inputX,inputY];

      var coeffMulDirection = 100;


      var distance=Math.sqrt(Math.pow(p1[0]-p2[0],2)+Math.pow(p1[1]-p2[1],2));
      if(distance < coeffMulDirection){
         coeffMulDirection = distance/4;
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

    svg = activeCanvas.find('svg');

    svg = svg[0];


    $("#drawingcanvas"+connectionid).remove();

    $("#drawingoutline"+connectionid).remove();


    var c1 = document.createElementNS("http://www.w3.org/2000/svg", "path");
    c1.setAttribute("id","drawingoutline"+connectionid);
    c1.setAttribute("stroke-width", "5");
    c1.setAttribute("stroke", bgcolor);
    c1.setAttribute("stroke-linejoin","round");
    c1.setAttribute("fill", "none");
    c1.setAttribute("rel", connectionid);
    c1.setAttribute("class", "drawingoutline"+connectionid);
    c1.setAttribute("d", "M"+bezierPoints[0][0]+","+bezierPoints[0][1]+" C"+bezierPoints[1][0]+","+bezierPoints[1][1]+" "+bezierPoints[2][0]+","+bezierPoints[2][1]+" "+bezierPoints[3][0]+","+bezierPoints[3][1]);
    svg.appendChild(c1);


    var c1 = document.createElementNS("http://www.w3.org/2000/svg", "path");
    c1.setAttribute("id","drawingcanvas"+connectionid);
    c1.setAttribute("stroke-width", "3");
    c1.setAttribute("stroke", color);
    c1.setAttribute("stroke-linejoin","round");
    c1.setAttribute("fill", "none");
    c1.setAttribute("rel", connectionid);
    c1.setAttribute("class", "drawingcanvases");
    c1.setAttribute("d", "M"+bezierPoints[0][0]+","+bezierPoints[0][1]+" C"+bezierPoints[1][0]+","+bezierPoints[1][1]+" "+bezierPoints[2][0]+","+bezierPoints[2][1]+" "+bezierPoints[3][0]+","+bezierPoints[3][1]);
    svg.appendChild(c1);

	updateConnectionListeners();

	//setTimeout(resetSelection,100);
	} else {
       // alert("test");
    }
}

/* this function is necesary because the library for drawing has bugs when drawing on a surface whose scrollPosition is not on the top left.
The scroll position is set to 0,0 before drawing and restored to the original position afterwards. */
function redrawLines() {
	for (c in connections) {
        drawConnection(c);
	}
	resizeWidgets();
}

/* updateConnectionListeners() should be called when a new connection has been drawn on the canvas.
All events concerning connections are described in this function.
It enables clicking on connections and changing their colors when
hovering over them with the mouse for easier understanding of the workflow. */
function updateConnectionListeners() {
	$(".drawingcanvases").unbind('mouseenter');
	$(".drawingcanvases").mouseenter(function() {
		connectionid = $(this).attr('rel');
        $(this).attr('stroke','#ffaaaa');
        $(".drawingoutline"+$(this).attr('rel')).attr('stroke','#ff0000');
	});

	$(".drawingcanvases").unbind('mouseleave');
	$(".drawingcanvases").mouseleave(function() {
		connectionid = $(this).attr('rel');
        if (selectedConnection!=($(this).attr('rel'))) {
        $(this).attr('stroke','#d1d1d1');
        $(".drawingoutline"+$(this).attr('rel')).attr('stroke','#a3a3a3');
        } else {
            $(this).attr('stroke','#ffaaaa');
            $(".drawingoutline"+$(this).attr('rel')).attr('stroke','#ff0000');
        }
	});

	$(".drawingcanvases").unbind('click');
	$(".drawingcanvases").click(function() {
		connectionid = $(this).attr('rel');
		selectedConnection=connectionid;
        $(this).attr('stroke','#ffaaaa');
		$(".widgetcenter").removeClass("ui-state-highlight");
		selectedWidget=-1;

		$(".drawingcanvases").each(function() {
			connectionid = $(this).attr('rel');
			currentLeft = $(this).css('left');
			currentTop=$(this).css('top');
			drawConnection(connectionid);
			$(this).css('top',currentTop);
			$(this).css('left',currentLeft);
		});
	});

}

function fileListeners() {
    $(".filename").change(function(){
        var input_id = $(this).attr('id').replace("id_file","");
        $("#upload_form"+input_id).submit();
    });
}

function stopUpload(result,input_id)
{
    unfinish($("#parameter_comment"+input_id).parent().parent().attr('rel'));
	$("#parameter_comment"+input_id).html(result);
	$(".filename").val("");

}

function resizeSvg() {
    $("svg").each(function() {
        $(this).css('height',$(this).parent()[0].scrollHeight-40+'px');
        $(this).css('width',$(this).parent()[0].scrollWidth-40+'px');
    });
}
