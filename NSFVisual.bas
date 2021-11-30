
'NSF player bus
#include "NSF.bi"
#include "NSF.bas"
#include once "windows.bi"
#include "AUDIO_NSF.bas"
#include "fbgfx.bi"

dim shared audio_hndler as AudioHandler ptr 
dim shared v as float

audio_hndler = new AudioHandler(48000,AUDIO_S16SYS,2048,16)

''NSF player bus
'#include "BUS.bi"
'#include "BUS.bas"

dim shared player as NSFplayer


dim shared paused as boolean = FALSE
dim shared loaded as boolean = FALSE 
dim shared pausedInBg as boolean = FALSE

'dim loopid as uint8_t
'const scrnwdth = 256
'const scrnhght = 240

'dim currentsong as uint8_t

screenres(256*3,240*3,32,2)
screenset(1,0)

dim shared keyprss(255) as boolean

	'Dim Shared NSFplayer1 As NSFplayer = NSFplayer()
	Dim Shared currentsong As uint8_t= 1	
MMapTemplate(UINT16T ,String)

Dim shared mapasm As  MAPNODEUINT16TSTRING Ptr
Dim shared map As  TMAPUINT16TSTRING

WIDTH 90, 40
WIDTH 90, 45
WIDTH 90, 46



declare SUB DrawAPU1 (x1 AS INTEGER, y1 AS INTEGER)
declare sub drawcode(x as integer ,y as integer ,nLiplayer as integer )
declare SUB DrawCpu (x1 AS INTEGER, y1 AS INTEGER)
declare sub drawtext1()
	'open "CONS:" for output as #1
	
locate ,,0	
'	Function  KEYPRESSED overload(vk_code As Integer) As bool
'	static Iskeyup(&HFF) As bool
'			
'	If  IIf(GetAsyncKeyState(vk_code) And &H8000, 1, 0) And iskeyup(vk_code) = TRUE Then
'		iskeyup(vk_code) = IIf(GetAsyncKeyState(vk_code) And &H8000, FALSE, TRUE)
'	Return TRUE
'	 
'	ElseIf IIf(GetAsyncKeyState(vk_code) And &H8000, 0, 1) And iskeyup(vk_code)= FALSE Then
'		iskeyup(vk_code) = IIf(GetAsyncKeyState(vk_code) And &H8000, FALSE, true)		
'		Return FALSE
'	End If
'	
'	Return FALSE			
'End Function

	Function  KEYPRESSED overload(_KEYS As Ubyte) As bool
	'static Iskeyup(&HFF) As bool
						if multikey(_KEYS) = true and keyprss(_KEYS) = true then
				

keyprss(_KEYS) = false
Return keyprss(_KEYS)
						elseif multikey(_KEYS) = false and keyprss(_KEYS) = false then
				
keyprss(_KEYS) = true
 
	Return keyprss(_KEYS)
						end if

	
	Return true	
	End Function

	Function hex1 (n As uint32_t,  d As uint8_t) As  string
	
		 Dim s As String = String(d, "0")
		 Dim i As Integer 
		
		'for (int i = d - 1; i >= 0; i--, n >>= 4)
		i = d-1
		While i >= 0
			s[i] = Asc("0123456789ABCDEF", (n And &Hf)+1)  '[n And &HF]
				n shr= 4
			   i-=1
		Wend
		'	s[i] = "0123456789ABCDEF"[n & &HF];
		'Next
		
		return s
	End Function

sub sound_data_test(offset as integer ,v  as float)
	
	
	'for i as integer = 0 to audio_hndler->samplesperframe - 1	
   audio_hndler->sampleBuffer(offset) = v'((rnd * 2-1) *0.2) 'v'cast(float,v)'v 
	'next
	
	
	
End Sub

	
sub _fillrect(x as integer,y as integer,w as integer,h as integer,col1 as long)
	
	if w <> 0  then
	if h <> 0 then
		
	line (x,y)-(w+x-1,h+y-1),col1,BF
	end if
	end if
	
End Sub



sub drawVisuals()
	
dim zoom as Integer

zoom = 3

'	

	drawAPU1(0,0)	
'	
'	
'	
'			 drawcpu(57,1)
'		drawcode(58,8,26)
'		   Locate 37, 3
'drawtext1()
'
'
'


'_fillrect(20,100,15 ,120,&H3f3f1f)
'
'_fillrect(35,100,15 ,120,&H1f1f3f)
'
'_fillrect(20,220,15 ,0,&Hffff7f)
'
'_fillrect(35, 100, 15, 10,&H7f7fff)

'pulse 1
'dim scale as uint32_t = iif(player.apu.p1ConstantVolume,player.apu.p1Volume,player.apu.p1Decay)
'scale = iif(player.apu.p1Counter = 0,0,scale)
'scale = scale * 120 / &HF
'_fillrect(20,100,15 ,120,&H3f3f1f)
'_fillrect(35,100,15 ,120,&H1f1f3f)
'_fillrect(20,220-scale,15 ,scale,&Hffff7f)
'scale = player.apu.p1Timer * 110 / &H7ff
'_fillrect(35, 100+scale, 15, 10,&H7f7fff)


dim scale as uint32_t = iif(player.apu.p1ConstantVolume,player.apu.p1Volume,player.apu.p1Decay)
scale = iif(player.apu.p1Counter = 0,0,scale)
scale = scale * 120 / &HF
_fillrect(20*zoom,100*zoom,15*zoom ,120*zoom,&H3f3f1f)
_fillrect(35*zoom,100*zoom,15*zoom ,120*zoom,&H1f1f3f)
_fillrect(20*zoom,(220-scale)*zoom,15*zoom ,scale*zoom,&Hffff7f)
scale = player.apu.p1Timer * 110 / &H7ff
_fillrect(35*zoom, (100+scale)*zoom, 15*zoom, 10*zoom,&H7f7fff)








''pulse 2
_fillrect(65*zoom,100*zoom,15*zoom ,120*zoom,&H3f3f1f)
_fillrect(80*zoom,100*zoom,15*zoom ,120*zoom,&H1f1f3f)
scale= iif(player.apu.p2ConstantVolume,player.apu.p2Volume,player.apu.p2Decay)
scale = iif(player.apu.p2Counter = 0,0,scale)
scale = scale * 120 / &HF
_fillrect(65*zoom,(220-scale)*zoom,15*zoom ,scale*zoom,&Hffff7f)
scale = player.apu.p2Timer * 110 / &H7ff
_fillrect(80*zoom, (100+scale)*zoom, 15*zoom, 10*zoom,&H7f7fff)

'triangle  
_fillrect(110*zoom,100*zoom,15*zoom ,120*zoom,&H3f3f1f)
_fillrect(125*zoom ,100*zoom,15*zoom ,120*zoom,&H1f1f3f)
scale = iif(player.apu.triCounter = 0 or player.apu.triLinearCounter = 0,0,1)
scale = scale * 120
_fillrect(110*zoom,(220-scale)*zoom ,15*zoom ,scale*zoom,&Hffff7f)
scale = player.apu.triTimer * 110 / &H7ff
_fillrect(125*zoom, (100+scale)*zoom, 15*zoom, 10*zoom,&H7f7fff)
'
'
''noise  
_fillrect(155*zoom,100*zoom,15*zoom ,120*zoom,&H3f3f1f)
_fillrect(170*zoom ,100*zoom,15*zoom ,120*zoom,&H1f1f3f)
scale= iif(player.apu.noiseConstantVolume,player.apu.noiseVolume,player.apu.noiseDecay)
scale = iif(player.apu.noiseCounter = 0,0,scale)
scale = scale * 120 / &HF
_fillrect(155*zoom,(220-scale)*zoom,15*zoom , scale*zoom,&Hffff7f)
scale = player.apu.noiseTimer * 110 / 4068
_fillrect(170*zoom, (100+scale)*zoom, 15*zoom, 10*zoom,&H7f7fff)
'


'SOON//////////////////////////////////////
'DMC
'_fillrect(200*zoom,100*zoom,15*zoom ,120*zoom,&H3f3f1f)
'_fillrect(215*zoom,100*zoom,15*zoom ,120*zoom,&H1f1f3f)
'_fillrect(200*zoom,220*zoom,15*zoom ,0*zoom,&Hffff7f)
'_fillrect(215*zoom, 100+109*zoom, 15*zoom, 10*zoom,&H7f7fff)
'
'///////////////////////////////////////////





pcopy 0,1
   
End Sub




sub runframe()
	
				
		''player.getSamples(audioHandler.sampleBuffer, audioHandler.samplesPerFrame)
		
		'for i as integer = 0 to audio_hndler->samplesperframe - 1
'			sound_data_test(i,((rnd * 2-1) * 0.2))
'next	
		
		

player.runframe()
player.getsamples(audio_hndler->sampleBuffer(), audio_hndler->samplesPerFrame)
audio_hndler->nextBuffer()
	
  drawvisuals()
	

	
'for i as integer = 0 to audio_hndler->samplesperframe - 1
'				v = (rnd * 2-1) *0.2
'audio_hndler->sampleBuffer(i) = v'(rnd * 2-1) *0.2
'next
'		
'print sizeof((rnd * 2-1) *0.2)
End Sub


sub pausing()
	
		if KEYPRESSED(FB.SC_SPACE) = false then
		'cls
		'do 
		'
		'player.cpu._clock()

		'Loop while player.cpu.complete() = false

	  ' player.runframe()
	 
	   
	paused = not(paused)
	''drawvisuals()
	'
	'runframe1
	'if paused = true and loaded = true then
 'runframe()
	'end if
	end if
	
	
	 if(paused = true and loaded = true) then
	  
     
  audio_hndler->start_aud() 
 'audio_hndler->start_aud()
    	runframe()
    
   
    
 'paused = false 
	 elseif paused = false and loaded = true then

 audio_hndler->stop_aud()
   '  paused = true
	 end if
	
End Sub
sub drawcode(x as integer ,y as integer ,nLiplayer as integer )
	
	








dim nOff as uint16_t

	 nOff=player.cpu.pc
	mapasm = map.findaddr(nOff)
	dim nLiney as Integer = (nLiplayer shr 1) * 1 + y
	
	color 255
	if mapasm <> null then
		color 10
		locate nLineY,x
			
	  print *mapasm->nData
	  'nOff+=1
	  color 255
	  while nliney < (nLiplayer *1) + y
	

	
	nOff+=1
	mapasm = map.findaddr(nOff)
	
	
	if mapasm <> null then
	nLineY += 1
	
	locate nLineY,x
	 print *mapasm->nData
	elseif nOff  = &HFFFF then
			nLineY += 1
	
	locate nLineY,x
	 print

	end if 
	 
	 
	wend 
	end if
	
	
		'mapasm = map.findaddr(player.cpu.pc)
	
	nliney = (nLiplayer shr 1) * 1 + y
	
	nOff=player.cpu.pc
	if mapasm <> null then

	  
	  while nliney > y
	
	
	
	nOff-=1
	mapasm = map.findaddr(nOff)
	
	
	if mapasm <> null  then
	nLineY -= 1	
	locate nLineY,x	 
	print *mapasm->nData
	elseif nOff = &HFFFF then
	nLineY -= 1	
	locate nLineY,x	 
	print 
	'	nOff-=1
	 'beep
	end if 
	 
	 
	  wend 
	end if

	

	

'next i
	
	
	
End Sub  

SUB DrawCpu (x1 AS INTEGER, y1 AS INTEGER)


    LOCATE y1, x1

Color 255
'11
    Print " status: ";
    Color(IIf(player.cpu.status And olc6502.N,10,12))
    PRINT " N ";
      Color(IIf(player.cpu.status And olc6502.V,10,12))
    PRINT " V ";
       Color(IIf(player.cpu.status And olc6502.U,10,12))
    PRINT " - ";
      Color(IIf(player.cpu.status And olc6502.B,10,12))
    PRINT " B ";
      Color(IIf(player.cpu.status And olc6502.D,10,12))
    PRINT " D ";
    Color(IIf(player.cpu.status And olc6502.I,10,12))
    PRINT " I ";
      Color(IIf(player.cpu.status And olc6502.Z,10,12))
    PRINT " Z ";
        Color(IIf(player.cpu.status And olc6502.C,10,12))
    PRINT " C "
    Color 255


    'Locate , x

   ' PRINT " ";getflag(N);" ";getflag(V);" ";getflag(U);" "; getflag(B);" ";getflag(D);" ";getflag(I);" "; getflag(Z);" ";getflag(C)
  
 '   Locate , x
   
    Locate y1+1, x1
    PRINT " PC: $"; LTRIM$(hex1(player.cpu.pc, 4)) + " [" + LTRIM$(STR$(player.cpu.pc)) + "]"
    Locate , x1
    PRINT " A: $"; LTRIM$(hex1(player.cpu.a, 2)) + " [" + LTRIM$(STR$(player.cpu.a)) + "]"
    Locate , x1
    PRINT " X: $"; LTRIM$(hex1(player.cpu.x, 2)) + " [" + LTRIM$(STR$(player.cpu.x)) + "]"
    Locate , x1
    PRINT " Y: $"; LTRIM$(hex1(player.cpu.y, 2)) + " [" + LTRIM$(STR$(player.cpu.y)) + "]"
    LOCATE , x1
    PRINT " Stack P: $"; LTRIM$(hex1(player.cpu.stkp, 4)) + " [" + LTRIM$(STR$(player.cpu.stkp)) + "]"




END Sub

SUB DrawAPU1 (x1 AS INTEGER, y1 AS INTEGER)
 Locate y1, x1
 
 	'print
 	color rgb(128+64,0,0)
 	'print "P1OUTPUT: ";player.apu._output(player.apu._outputoffset)
 	'
 	'print
 	'print audio_hndler->sampleBuffer(14)
 	'	print
 	
 	'	print "P1OUTPUT: ";player.apu.p1output
 	'print "P1OUTPUT: ";player.apu.p2output
 	'print "
 	'
 	
 	
 	'print
	'Print "Title: ";player.nsffile.namesong 

	'Print "Artist: ";player.nsffile.artist 

	'Print "Copyright: ";player.nsffile.copyright 
   'print
   '
   'Print "banked: "; Iif(player.nsffile.banking,TRUE,FALSE)
   print
	Print "Song ";currentsong;" of ";player.NSFfile.totalsongs 




'LOCATE y1, x1
'
'print "NOISE ENABLED: ";player.apu.enableNoise
'print "TRIANGLE ENABLED: ";player.apu.enableTriangle  
'print "PULSE1 ENABLED: ";player.apu.enablePulse1 
'print "PULSE2 ENABLED: ";player.apu.enablePulse2
'print "DMC ENABLED: ";player.apu.enablePulse2
'print "P1 OUTPUT: ";player.apu.p1output ' WORKING PROPERLY
'print "P2 OUTPUT: ";player.apu.p2output ' WORKING PROPERLY
'print "P1 COUNTER: ";player.apu.p1counter ' WORKING PROPERLY
'print "P2 COUNTER: ";player.apu.p2counter  ' WORKING PROPERLY

print "P1TIMER: ";player.apu.p1timer
print "P2TIMER: ";player.apu.p2timer
print "P1 SWEEP TARG: ";player.apu.p1SweepTarget
print "P2 SWEEP TARG: ";player.apu.p2SweepTarget
print
print "output: ";player.apu._output(0)



'print "TRI COUNTER: ";player.apu.tricounter ' WORKING PROPERLY
'print "NOISE COUNTER: ";player.apu.noisecounter ' WORKING PROPERLY
'print
'print "STEP 5 MODE: ";player.apu.step5mode ' WORKING PROPERLY
'print "INTERRUPTINHIBIT: ";player.apu.interruptInhibit ' WORKING PROPERLY
'print "FRAMECOUNTER: ";player.apu.framecounter' ' WORKING PROPERLY
'



END Sub
SUB DrawRam (x1 AS INTEGER, y1 AS INTEGER, addr AS uint16_t, nRows AS INTEGER, nColumns AS INTEGER)
	
	
	Color 255
	
    Dim nRamX As Integer = x1
     Dim nRamY As Integer = y1
     Dim sOffset As String
     Dim row As Integer
     Dim col As Integer 
    ' Dim addr As uint16_t

    FOR row = 0 TO nRows - 1
        sOffset = "$" + hex1(addr, 4) + ":"
        FOR col = 0 TO nColumns - 1


            sOffset = sOffset  + " " + hex1(player._read(addr, TRUE),2)

            addr+=1

        NEXT col

        LOCATE nRamY, nRamX


        PRINT sOffset
        nRamY = nRamY + 1
    NEXT row

END SUB


sub drawtheram()
	
		DrawRam 2, 1, &H0000, 16, 16

    DrawRam 2, 19, &H3FF0, 16, 16
End Sub


sub drawAPUvals()
	

	print "test"
	
	
	
End Sub

sub drawtext1()
		 drawcpu(57,1)
		drawcode(58,8,26)
		    Locate 37, 3
'
'
'
'
'
  print  "SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI" ;
   Locate 38, 3
print  "N - next song    P - PREV song     R - RESET song     O - OPEN new song" ;

Locate 39, 3
print  "S - PAUSE/PLAY song";
	

 Locate 40, 3
 	print
 	Locate 41, 3
	Print "Title: ";player.nsffile.namesong;
	Locate 42, 3
	Print "Artist: ";player.nsffile.artist;
Locate 43, 3
	Print "Copyright: ";player.nsffile.copyright;
	Locate 44, 3
	Print "Song ";currentsong;" of ";player.NSFfile.totalsongs;
Locate 45, 3
	Print "banked: "; Iif(player.nsffile.banking,TRUE,FALSE)

End Sub
	
sub update()
	
	dim looping as boolean
	 

screen ,1,0
cls
screen ,0,1
cls
    'runframe()
   
   looping = true
   'paused = false
   'print FB.SC_N
   'pcopy 0 ,1
   'sleep  

   'currentsong = 11
   player.playsong(currentsong)
drawVisuals()



	do
		cls
		
		'player.apu.out1.apu_output(0)'
		
		' audio_hndler->sampleBuffer(1)
		'print player.apu.out1.apu_output(player.out1.outputOffset)
		
		
		
		  dim as uint64_t start1 =  SDL_GetPerformanceCounter() 
 
     
      pausing()

		if KEYPRESSED(FB.SC_R) = false then
   
	player.playsong(currentsong)
	drawVisuals()
		end if

		
	
'	_KEY = FB.SC_N
'			if multikey(_KEY) = true and keyprss(_KEY) = true then
'				
'				cls
'				currentsong+=1 
'				
'   currentsong = iif(currentsong > player.NSFfile.totalsongs,player.NSFfile.totalsongs,currentsong)
'   
'	player.playsong(currentsong)
'		
' drawvisuals()
'
'keyprss(_KEY) = false
'
'			elseif multikey(_KEY) = false and keyprss(_KEY) = false then
'				
'keyprss(_KEY) = true
' 
'	
'			end if
'			
			
			
				if KEYPRESSED(FB.SC_P) = false then
				cls
				currentsong-=1 
				
   currentsong = iif(currentsong < 1,1,currentsong)
   
player.playsong(currentsong)
	
		
  drawvisuals()
				end if
				
'	runframe()			
 'drawvisuals()
 '

 if KEYPRESSED(FB.SC_N) = false then
 	
 					cls
				currentsong+=1 
				
   currentsong = iif(currentsong > player.NSFfile.totalsongs,player.NSFfile.totalsongs,currentsong)
   
	player.playsong(currentsong)
		
 drawvisuals()


 	
 	
 	
 	
 end if
 '
 
 	
 	
 	dim as uint64_t end1 	= SDL_GetPerformanceCounter()
 	
 	
     dim elapsed  as float = (end1 - start1) / cast(float,SDL_GetPerformanceFrequency())
            dim elapsedMS as float = (end1 - start1) / cast(float,SDL_GetPerformanceFrequency()) * 1000.0f

          SDL_Delay(int(16.666f - elapsedMS))
	loop until inkey() = "q" or looping = false
	
End Sub
	Sub loadNSFRom(filenme As String)
		
	
	
		If player.LoadNSFile(filenme,map) Then
		
			
			If loaded = false and paused = false Then
				  ' paused = not(paused)
				   
					loaded = TRUE
			currentsong = player.NSFfile.startsong 
			    audio_hndler->start_aud()
			    
			    
			
				update()
	    
				
		End if
		elseif filenme = "" then
			 print"invalid NSF file"
 pcopy 1,0
  sleep
	
		
			
		EndIf
		
		
	
		
		
		
		
		
		
		
End Sub
	sub init_nsf()

	'NSFplayer = NSFplayer()
  '  loadNSFRom("Super Mario Bros. 3 (1988-10-23)(Nintendo EAD)(Nintendo).nsf") '- WORKS
	loadNSFRom("C:\Users\Gamer\Desktop\Silver Surfer (EMU).zophar\Silver Surfer (SFX).nsf")' - WORKS
	
	 '  loadNSFRom("Dr. Mario (1990-07-27)(Nintendo R&D1)(Nintendo).nsf") 
	'loadNSFRom("Mega Man 2 [RockMan 2 - Dr. Wily no Nazo] (1988-12-24)(Capcom).nsf")' - WORKS
	'loadNSFRom("SMB.nsf")

'	If  Then
		
'	Else
	'	loadNSFRom("Castlevania 2 - Simon's Quest [Dracula 2 - Noroi no Fuuin] (1988-12)(Konami).nsf")
		
		
	'EndIf
	'loadNSFRom("Incredible Crash Dummies, The (1993-09-23)(Software Creations)(LJN).nsf") - WORKS
	'loadNSFRom("Tiny Toon Adventures (1991-12-20)(Konami).nsf") - WORKS
	
	'loadNSFRom("Yoshi's Cookie (1992-11-21)(Tose)(Nintendo).nsf") '- WORKS
	'loadNSFRom("Kirby's Adventure [Hoshi no Kirby - Yume no Izumi no Monogatari] (1993-03-23)(HAL Laboratory)(Nintendo).nsf") - WORKS
 
 
 'loadNSFRom("Journey to Silius [RAF World] (1990-08-10)(Sunsoft).nsf")
 'loadNSFRom("Felix the Cat (1992-10)(Shimada Kikaku)(Hudson).nsf") 
 
 'loadNSFRom("Mario is Missing! (1993-07)(Radical)(Software Toolworks).nsf") 
 
 
 

	
	End Sub


init_nsf



    SDL_CloseAudioDevice(audio_hndler->audio_device)
    delete audio_hndler
    audio_hndler = null
     SDL_Quit()

'sleep