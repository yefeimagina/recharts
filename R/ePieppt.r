ePie = function(dat, namevar=NULL, datavar=NULL, size = NULL,  type=c("pie", "rose"), roseType=c("radias", "area"),
	theme = "default", title = NULL, subtitle = NULL, title.x = "center", title.y = "top", 
	legend = TRUE, legend.x = "left", legend.y= "top", legend.orient="horizontal", 
	toolbox = TRUE, toolbox.orient = "horizontal", toolbox.x = "right", toolbox.y = "top", 
	dataView = TRUE, readOnly = FALSE, mark=TRUE, dataZoom=FALSE, magicType=FALSE,
	tooltip = TRUE, tooltip.trigger="item", formatter="", axis.scale=TRUE,
	xlab=FALSE, ylab=FALSE,	calculable=TRUE, showLabel=TRUE, opt = list(),rad1 = 20,rad2 = 100,lel = 6)
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
			radius = c(rad1,rad2),
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
	for(i in 1:lel)
{
 opt$series[[1]]$data[[i]]$name = paste0(opt$series[[1]]$data[[i]]$name," : ",opt$series[[1]]$data[[i]]$value)
}

	### output list format
	chart = htmlwidgets::createWidget(
		'echarts', opt, width = size[1], height = size[2], package = 'recharts'
	)
	chart = .addClass(chart, "ePie")
	 #add theme dependencies
	chart = addThemeDependencies(chart)
	chart
}
