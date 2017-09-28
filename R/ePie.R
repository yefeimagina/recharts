ePie = function(dat, namevar=NULL, datavar=NULL, size = NULL,  type=c("pie", "rose"), roseType=c("radias", "area"),
	theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top", 
	legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal", 
	toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top", 
	dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=FALSE,
	tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=TRUE,
	xlab=FALSE, ylab=FALSE,	calculable=TRUE, showLabel=TRUE, opt = list(),re1 = 20,re2 = 100,ty = 1,char = "{a} :{b}")
{
	type <- match.arg(type)
	roseType <- match.arg(roseType)
	# if the input is an array or a vector, will use the names as the pie name,
	# and use the value as the pie value
	if(is.vector(dat) || is.array(dat)){
		dat <- as.data.frame(dat)
		datavar <- 1
		dat$namevar <- rownames(dat)
		namevar <- "namevar"
	}
	else{
		# if the input dat is not data.frame will format it into data.frame.
		if (class(dat) != "data.frame") dat <- as.data.frame(dat)
		
		# if the user input argument namevar is null, will use colume one as name input
		if(is.null(namevar)){
			namevar <- 1
		}else{
			namevar = autoArgLabel(namevar, deparse(substitute(namevar)))
			namevar = evalFormula(namevar, data)
		}
		if(is.null(datavar)){
			datavar <- 2
		}else{
			datavar = autoArgLabel(datavar, deparse(substitute(datavar)))
			datavar = evalFormula(datavar, data)
		}
	}

	# option$title format.
	opt$title = tilteSet(title = title, subtitle=subtitle,
			title.x = title.x, title.y = title.y)
	
	opt$calculable = calculableSet(calculable = calculable)
	opt$theme = themeSet(theme = theme)

	# opt$tooltip format, not open to user now.
	opt$tooltip = tooltipSet( tooltip=tooltip,trigger=tooltip.trigger,
			formatter = "", islandFormatter="")
	
	opt$toolbox = toolboxSet(toolbox=toolbox, toolbox.x=toolbox.x, toolbox.y=toolbox.y, orient=toolbox.orient,
				dataView=dataView, mark=mark, dataZoom = dataZoom, magicType = magicType, restore = TRUE, readOnly = readOnly,
				saveAsImage=TRUE)

				
	opt$legend = legendSet( show=legend, data=dat[[namevar]], legend.x=legend.x, legend.y=legend.y, orient=legend.orient)
	
	datFrame = data.frame(value=dat[[datavar]], name=dat[[namevar]])
    datList = lapply(split(datFrame, seq_len(nrow(datFrame))), as.list)
    names(datList) = NULL
	
	#showLabelLine=showLabel
	#now we don't support the multiple graph in one canvas
	opt$series = list(
		list(
			name = paste(type, "chart"),
			type = "pie",
			radius = c(re1,re2),
			center = c("50%", 200),
			roseType = ifelse(type=="rose", roseType, ""),
			itemStyle = list(
				normal = list(
					label = list( show = showLabel),
					labelLine = list( show = showLabel)
				),
				emphasis = list(
					label = list( show = !showLabel),
					labelLine = list( show = !showLabel)
				)
			),
			data = datList
		)
	)
	
	#jsonStr <- toJSON(opt, pretty=TRUE)
	#outList <- .rechartsOutput(jsonStr, charttype="ePie", size=size)
	opt$size = size
        if(ty == 1) {
	### output list format
	chart = htmlwidgets::createWidget(
		'echarts', opt, width = size[1], height = size[2], package = 'recharts'
	)
	chart = .addClass(chart, "ePie")
	# add theme dependencies
	chart = addThemeDependencies(chart)
	chart
	}else{
		a = opt
		
		
a$series[[1]]$label = list()
a$series[[1]]$label$normal= list()
a$series[[1]]$label$normal$show = TRUE
a$series[[1]]$label$normal$position = 'center'
a$series[[1]]$label$normal$formatter = char


a$series[[1]]$emphasis = list()
a$series[[1]]$label$emphasis$show = TRUE
a$series[[1]]$label$emphasis$textStyle = list()
a$series[[1]]$label$emphasis$textStyle$fontSize = 30
a$series[[1]]$label$emphasis$textStyle$fontWeight = 'bold'

chart = htmlwidgets::createWidget(
'echarts', a, width = a$size[1], height = a$size[2], package = 'recharts')
chart
	}
}

