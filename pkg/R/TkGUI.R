################################
# CLI for prepRISA function
################################
"prepRISA.CLI" <- function ()
{
#
# user dialogs
#
	cat("Data file (no def.) ? ")
	filename <- readLines(n=1)
	if (filename == "") stop("Filename needed")
	
	cat("Number of groups (def. = 1) ? ")
	ngr <- as.integer(readLines(n=1))
	if (is.na(ngr)) ngr <- 1
	
	cat("Sum (def. = 1) or Max (2) of peaks ? ")
	ioptsm <- as.integer(readLines(n=1))
	if (is.na(ioptsm)) ioptsm <- 1
	
	cat("Row percentage tranformation (def. = yes = 1) ? ")
	ioptpt <- as.integer(readLines(n=1))
	if (is.na(ioptpt)) ioptpt <- 1
	
	cat("Max. number of peaks (def. = All = 0) ? ")
	maxnp <- as.integer(readLines(n=1))
	if (is.na(maxnp)) maxnp <- 0
	
	cat("Start (def. = mini = 0) ? ")
	startp <- as.integer(readLines(n=1))
	if (is.na(startp)) startp <- 0

	claw <- vector(mode="numeric",length=ngr)
	maxl <- vector(mode="numeric",length=ngr-1)
#
# follow up on user dialogs
#
	for (i in 1:ngr) {
		cat("Group number",i,":\n")
		cat("Class width  (def. = 2) ? ")
		claw[i] <- as.integer(readLines(n=1))
		if (is.na(claw[i])) claw[i] <- 2
	}

	if (ngr>1) {
		for (i in 1:(ngr-1)) {
			cat("Group number",i,":\n")
			cat("Upper limit  ? ")
			maxl[i] <- as.integer(readLines(n=1))
		}
	}
	
	prepRISAfun(filename, ngr, ioptsm, ioptpt, maxnp, startp, claw, maxl)
}

################################
# GUI for prepRISA function
################################
"prepRISA.GUI" <- function()
{
	require(tcltk) || stop("tcltk support is absent")

	claw <- NULL
	maxl <- NULL

	op <- options()
	options(warn=-1)
#
# Main dialog window with title
#
	tt <- tktoplevel()
	tkwm.title(tt,"prepRISA")	
#
# Variables for text fields
#
	invar <- tclVar()
	outvar <- tclVar("tabRISA")
	ngvar <- tclVar(1)
	maxvar <- tclVar(0)
	startvar <- tclVar(0)
	typesvar <- tclVar()
#
# Checkboxes
#
	rpvar <- tclVar(1)
	smvar <- tclVar(1)
#
# Title
#
	TFrame <- tkframe(tt, relief="groove")
	labh <- tklabel(TFrame, bitmap="questhead")
	tkgrid(tklabel(TFrame,text="prepRISA", font="Times 18", foreground="red"), labh)
	tkbind(labh, "<Button-1>", function() print(help("prepRISA")))
	tkgrid(TFrame)
#
# input RISA datafile and output dataframe
#	
	IOFrame <- tkframe(tt, relief="groove", borderwidth=2)
	tkgrid(tklabel(IOFrame,text="- Input & output -", foreground="blue"), columnspan=5)
	in.entry <- tkentry(IOFrame, textvariable=invar)
	out.entry <- tkentry(IOFrame, textvariable=outvar)
	choosedf.but <- tkbutton(IOFrame, text="Set", command=function() choosefile())
	tkgrid(tklabel(IOFrame,text="RISA data file : "), in.entry, choosedf.but, sticky="w")
	tkgrid(tklabel(IOFrame,text="Output dataframe : "), out.entry, sticky="w")
	tkgrid(IOFrame)
#
# misc options
#
	miscFrame <- tkframe(tt, relief="groove", borderwidth=2)
	tkgrid(tklabel(miscFrame,text="- options -", foreground="blue"), columnspan=2)
	ng.entry <- tkentry(miscFrame, textvariable=ngvar, width=6)
	max.entry <- tkentry(miscFrame, textvariable=maxvar, width=6)
	start.entry <- tkentry(miscFrame, textvariable=startvar, width=6)
	tkgrid(tklabel(miscFrame,text="Number of groups : "), ng.entry, sticky="w")
	tkgrid(tklabel(miscFrame,text="Max number of peaks (0 = all) : "), max.entry, sticky="w")
	tkgrid(tklabel(miscFrame,text="Starting position (0 = min.): "), start.entry, sticky="w")
	tkgrid(miscFrame)
#
# Percentage tansformation
#
	RPFrame <- tkframe(tt, relief="groove", borderwidth=2)
	tkgrid(tklabel(RPFrame,text="- Row percentage transformation -", foreground="blue"), columnspan=3)
	rp.cbut <- tkcheckbutton(RPFrame,text="Row percentage tranformation    ", variable=rpvar)
	tkgrid(rp.cbut, sticky="w", columnspan=3)
	tkgrid(RPFrame)
#
# Sum or Max
#	
	SMFrame <- tkframe(tt, relief="groove", borderwidth=2)
    tkgrid(tklabel(SMFrame, text="- Use the sum or maximum of peaks -", foreground="blue"), columnspan=2)
    tkgrid(tkradiobutton(SMFrame, text="Sum", value=1, variable=smvar), sticky="w")
    tkgrid(tkradiobutton(SMFrame, text="Max", value=2, variable=smvar), sticky="w")
	tkgrid(SMFrame)
#
# Local variables
#
	done <- tclVar(0)	# To terminate the dialog
	
	"choosefile" <- function()
	{
		tclvalue(typesvar) <- "{{Text files} {.txt}}"
		fictrt <- tkgetOpenFile(filetypes=tclvalue(typesvar))
		fpath <- tclvalue(fictrt)
		tkdelete(in.entry, 0, "end")
		tkinsert(in.entry, "end", fpath)
	}
	
################################
# Function to build the command line from dialog widgets
################################
	"build" <- function()
	{
	#
	# Check that the input data file is not empty and get its name
	#
		if (tclvalue(invar) != "") {
			inpfile  <- tclvalue(invar)
		} else {
			return(0)
		}
	#
	# Get row percentage checkboxe state
	#
		rpercl <- as.logical(tclObj(rpvar))
		if (rpercl) rperc <- 1 else rperc <- 0
		sumax <- as.integer(tclvalue(smvar))
	#
	# Get options
	#
		if (tclvalue(ngvar) != "") {
			ng <- as.integer(tclvalue(ngvar))
		} else ng <- 1
		if (tclvalue(maxvar) != "") {
			maxnp <- as.integer(tclvalue(maxvar))
		} else maxnp <- 0
		if (tclvalue(startvar) != "") {
			startp <- as.integer(tclvalue(startvar))
		} else startp <- 0
	#
	# Make the command line
	#
		if (suite(ng)) substitute(prepRISAfun(inpfile, ng, sumax, rperc, maxnp, startp, claw, maxl))
	}
		
################################
# Function to reset all dialog elements to default values
################################
	"reset" <- function()
	{
		tclvalue(invar)<-""
		tclvalue(outvar)<-""
		tclvalue(ngvar)<-"1"
		tclvalue(maxvar)<-"0"
		tclvalue(startvar)<-"0"
		tclvalue(rpvar)<-"1"
		tclvalue(smvar)<-"1"
	}
	
################################
# Function to launch computations
################################
	"execcomp" <- function()
	{
	#
	# Check that the analysis name is not empty and get it
	#
		dfname <- parse(text=paste("\"",tclvalue(outvar)[[1]],"\"",sep=""))
	#
	# Build and display the command line so that the user can check it
	#
		cmd <- build()
		if (cmd == 0) return(0)
		#
		# Echoe the command line to the console
		#
#		pr1 <- substr(options("prompt")$prompt, 1,2)
#		cat(eval(dfname), " <- ", deparse(cmd, width = 256), "\n", pr1, sep="")
		#
		# Execute the command
		#
		myCom <- eval.parent(cmd)
		assign(eval(dfname), myCom, pos=1)
#		commande = paste(eval(dfname), " <- ", deparse(cmd, width = 500), sep = "")
#		rewriteHistory(commande)
	}
#
# Reset Cancel and Submit buttons
#
	RCSFrame <- tkframe(tt, relief="groove")
	reset.but <- tkbutton(RCSFrame, text="Reset", command=reset)
	cancel.but <- tkbutton(RCSFrame, text="Dismiss", command=function() tkdestroy(tt))
	submit.but <- tkbutton(RCSFrame, text="Submit", default="active", command=function() execcomp())
	tkgrid(cancel.but, submit.but, reset.but, ipadx=20)	
	tkgrid(RCSFrame)
#
# If window is closed by user, terminate the dialog
#
	tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)
	tkbind(tt, "<KeyPress-Return>", function() execcomp())
	tkbind(tt, "<KeyPress-Escape>", function() tkdestroy(tt))
#
# User closed the window
#
	if(tclvalue(done)=="2") return()


#
# Group parameters dialog window
#
	"suite" <- function(ngr=1)
	{
		op <- options()
		options(warn=-1)
	#
	# Main dialog window with title
	#
		tt <- tktoplevel()
		tkwm.title(tt,"Group parameters")	
	#
	# Variables for text fields (thanks to Peter Delgaard :)
	# http://finzi.psych.upenn.edu/R/Rhelp02a/archive/71791.html
	# [R] Building tkentry dynamicly from Peter Dalgaard on 2006-03-07 (2006-March.txt)
	#
		cwvar <- vector("list",ngr)
		for (i in 1:ngr) cwvar[[i]] <- tclVar("2")
		if (ngr>1) {
			ulvar <- vector("list",ngr-1)
			for (i in 1:(ngr-1)) ulvar[[i]] <- tclVar("")
		}
	#
	# Title
	#
		TFrame <- tkframe(tt, relief="groove")
		labh <- tklabel(TFrame, bitmap="questhead")
		tkgrid(tklabel(TFrame,text="Group parameters", font="Times 18", foreground="red"), labh)
		tkbind(labh, "<Button-1>", function() print(help("prepRISA")))
		tkgrid(TFrame)
	#
	# options
	#
		cwFrame <- tkframe(tt, relief="groove", borderwidth=2)
		tkgrid(tklabel(cwFrame,text="- Class width -", foreground="blue"), columnspan=2)
		cw.entry <- vector("list",ngr)
		for (i in 1:ngr) {
			cw.entry[[i]] <- tkentry(cwFrame, textvariable=cwvar[[i]])
			tkgrid(tklabel(cwFrame,text=paste("Group # : ",i,sep="")), cw.entry[[i]], sticky="w")
		}
		tkgrid(cwFrame)

		if (ngr>1) {
			ulFrame <- tkframe(tt, relief="groove", borderwidth=2)
			tkgrid(tklabel(ulFrame,text="- Upper limit -", foreground="blue"), columnspan=2)
			ul.entry <- vector("list",length=ngr-1)
			for (i in 1:(ngr-1)) {
				ul.entry[[i]] <- tkentry(ulFrame, textvariable=ulvar[[i]])
				tkgrid(tklabel(ulFrame,text=paste("Group # : ",i,sep="")), ul.entry[[i]], sticky="w")
			}
			tkgrid(ulFrame)
		}
		
		done <- tclVar(0)	# To terminate the dialog
		
		"getGroupParams" <- function()
		{
			lclaw <- vector(mode="numeric",length=ngr)			
			for (i in 1:ngr) {
				if (tclvalue(cwvar[[i]]) != "") {
					lclaw[i] <- as.integer(tclvalue(cwvar[[i]]))
				} else  return(0)
			}
			claw <<- lclaw				
			
			if (ngr>1) {
				lmaxl <- vector(mode="numeric",length=ngr-1)
				for (i in 1:(ngr-1)) {
					if (tclvalue(ulvar[[i]]) != "") {
						lmaxl[i] <- as.integer(tclvalue(ulvar[[i]]))
					} else return(0)
				}
				maxl <<- lmaxl
			}
			tclvalue(done) <- 2
		}
		
################################
# Function to reset all dialog elements to default values
################################
		"reset" <- function()
		{
			for (i in 1:ngr) tclvalue(cwvar[[i]]) <- "2"
			if (ngr>1) {
				for (i in 1:(ngr-1)) tclvalue(ulvar[[i]]) <- ""
			}
		}
		
	#
	# Reset Cancel and Submit buttons
	#
		RCSFrame <- tkframe(tt, relief="groove")
		reset.but <- tkbutton(RCSFrame, text="Reset", command=reset)
		cancel.but <- tkbutton(RCSFrame, text="Dismiss", command=function() tkdestroy(tt))
		submit.but <- tkbutton(RCSFrame, text="Submit", default="active", command=function() getGroupParams())
		tkgrid(cancel.but, submit.but, reset.but, ipadx=20)	
		tkgrid(RCSFrame)
	#
	# If window is closed by user, terminate the dialog
	#
		tkbind(tt, "<Destroy>", function() {return(0)})
		tkbind(tt, "<KeyPress-Return>", function() getGroupParams())
		tkbind(tt, "<KeyPress-Escape>", function() tkdestroy(tt))
	#
	# User closed the window
	#
		tkwait.variable(done)
		if(tclvalue(done)=="2") {
			tkdestroy(tt)
			return(1)
		}
	}
}
