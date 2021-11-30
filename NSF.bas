#pragma once
#include "olc6502.bi"


'print "in NSFplayer.bas"



constructor NSFplayer()






cpu.ConnectBus(@this)
apu.ConnectBus(@this)


this.playreturned = TRUE
this.frameirqwanted = FALSE
'this.dmcirqwanted = FALSE

erase this.ram


End Constructor


'FINISHED
Function NSFplayer.LoadNSFile(NSF As String,byref disasm as TMAPUINT16TSTRING) As BOOLEAN

	
	if nsf = "" then
		print "invalid NSF file"
		sleep
		return false
	EndIf
	
	
	
	Dim nsf1 As Integer = FreeFile
		
	Open NSF For Binary As nsf1
	'
	'Get #nsf1,,nsffile.format
	'Print nsffile.format
	'
	'Get #nsf1,,nsffile.version
	'	Print nsffile.version
	'
	'	Get #nsf1,,nsffile.totalsongs
	'	Print nsffile.totalsongs
	'	
	'	Get #nsf1,,nsffile.startsong
	'	Print nsffile.startsong
	'	
	'	
	'		
	'Get #nsf1,,nsffile.loadaddr
	'Print Hex(nsffile.loadaddr)
	'Get #nsf1,,nsffile.initaddr
	'Print Hex(nsffile.initaddr)
	'Get #nsf1,,nsffile.playaddr
	'Print Hex(nsffile.playaddr)	
	'	
	'Get #nsf1,,nsffile.Namesong
	'Print nsffile.Namesong	
   '
   'Get #nsf1,,nsffile.artist
	'Print nsffile.artist
   '
   'Get #nsf1,,nsffile.copyright
	'Print nsffile.copyright
   '
   'Seek #nsf1,2
   '
   'Get #nsf1,,nsffile.initbanks()
	'Print nsffile.initbanks(0)
   'Print nsffile.initbanks(1)
   'Print nsffile.initbanks(2)
   'Print nsffile.initbanks(3)
   'Print nsffile.initbanks(4)
   'Print nsffile.initbanks(5)
   'Print nsffile.initbanks(6)
   'Print nsffile.initbanks(7)
   'Seek #nsf1,2
   'Seek #nsf1,1
   'Seek #nsf1,1
   'Seek #nsf1,1
   '
   '
   'Get  #nsf1,,*nsffile.con_progdata,3 
   'Print " " 
   '
   '	'Print nsffile.con_progdata
   ''	nsffile.con_progdata+=1
   '	'Print nsffile.con_progdata(0]
   '	'Print nsffile.con_progdata(1]
   '	'Print nsffile.con_progdata(2]
   ''	Print nsffile.con_progdata
   	

	'nsffile.banking = IIf(nsffile.total > 0,TRUE,FALSE)
 
 
 
	'Print Iif(nsffile.banking,TRUE,FALSE)
	
	'Seek #nsf1,1 
'works correctly////////////////////////////////////////
	nsffile.rom_data.resize(Lof(nsf1),0)
	Get  #nsf1,,nsffile.rom_data[0],Lof(nsf1)
	Close nsf1
	
	if(nsffile.rom_data.size() < &H80) Then
		
	   Print "Invalid NSF loaded"  
	     sleep  
	    return FALSE
		
		
	EndIf
	
	    if  nsffile.rom_data[0] <> &H4E Or  nsffile.rom_data[1] <> &H45 _
	    Or nsffile.rom_data[2] <> &H53 Or nsffile.rom_data[3] <> &H4D Or nsffile.rom_data[4] <> &H1a then
	    
	   Print "Invalid NSF loaded"
	   

	return false
	    End If
	
	    if nsffile.rom_data[5] <> 1    Then
	    
	         Print "Unknown NSF version: " & nsffile.rom_data[5]    
	           return FALSE
	           
	    End If
	    
	    
	    nsffile.totalsongs   = nsffile.rom_data[6]
	    nsffile.startsong   = nsffile.rom_data[7]
	    
	   NSFfile.loadaddr = nsffile.rom_data[8] Or (nsffile.rom_data[9] shl 8)
	     if(NSFfile.loadaddr < &H8000) Then
	     	Print "Load address less than 0x8000 is not supported"
	     	 return FALSE
	     EndIf
	
	nsffile.initAddr   = nsffile.rom_data[&HA] or (nsffile.rom_data[&HB] shl 8)
	nsffile.playAddr  = nsffile.rom_data[&HC] or (nsffile.rom_data[&HD] shl 8)
	
	
	Dim name1 As String
	
	
	For i As uint8_t = 0 To 32-1
		If nsffile.rom_data[&H0E + i] = 0 Then
			
			Exit for
		end if
			
			nsffile.namesong +=  Chr(nsffile.rom_data[&H0E + i])
		
		
	Next
	
	'Print nsffile.namesong
	
		For i As uint8_t = 0 To 32-1
		If nsffile.rom_data[&H2E + i] = 0 Then
			
			Exit for
		EndIf
			
			nsffile.Artist +=  Chr(nsffile.rom_data[&H2E + i])
		
		
	Next
'	Print nsffile.Artist
	
	
			For i As uint8_t = 0 To 32-1
		If nsffile.rom_data[&H4E + i] = 0 Then
			
			Exit for
		EndIf
			
			nsffile.copyright +=  Chr(nsffile.rom_data[&H4E + i])
		
		
	Next
	'Print nsffile.copyright
	
	
	Erase nsffile.initbanks


	For i As uint8_t = 0 To 8 - 1
		
		nsffile.initbanks(i) = nsffile.rom_data[&H70 + i]
		nsffile.total = nsffile.rom_data[&H70 + i]
		
	Next
	
	nsffile.banking = IIf(nsffile.total > 0,TRUE,FALSE)
'//////////////////////////////////////////////////////////////

	this.mapper = New  nsfmapper(nsffile.rom_data,NSFfile.loadaddr,NSFfile.banking,NSFfile.initbanks())
	
'works correctly/////////////////////////////////////////////////	
	'what is a callarea?
	    callArea(0) = &H20'; // JSR    
	    callArea(1) = nsffile.initAddr and &Hff';   
	    callArea(2) = nsffile.initAddr Shr 8';    
	    callArea(3) = &Hea '// NOP    
	    callArea(4) = &Hea' // NOP    
	    callArea(5) = &Hea' // NOP    
	    callArea(6) = &Hea' // NOP    
	    callArea(7) = &Hea '// NOP    
	    callArea(8) = &H20'; '// JSR   
	    callArea(9) = nsffile.playAddr and &Hff';    
	    callArea(&Ha) = nsffile.playAddr Shr 8';    
	    callArea(&Hb) = &Hea '// NOP   
	    callArea(&Hc) = &Hea '// NOP   
	    callArea(&Hd) = &Hea '// NOP   
	    callArea(&He) = &Hea '// NOP  
	    callArea(&Hf) = &Hea '// NOP
'////////////////////////////////////////////////////////////////	  

 'print this.mapper->banked
 ' 
 ' print this.mapper->banks(0)
 ' print this.mapper->banks(1)
 ' print this.mapper->banks(2)
 ' print this.mapper->banks(3)
 ' print this.mapper->banks(4)
 ' print this.mapper->banks(5)
 ' print this.mapper->banks(6)
 ' print this.mapper->banks(7)

 ' print this.mapper->maxBanks

  
  'print this.mapper->prgram(0)
  'print this.mapper->romdata[0]

	disasm = this.cpu.disassemble(&H0000,&HFFFF)

	playsong(nsffile.startsong)
	
	
	'Print "Loaded NSF file"
	Return TRUE
	
	

End function

''FINISHED for now
'Sub NSFplayer.playsong(songNum As uint8_t)
'		
'		'reset////////////////////////////////
'		Erase this.ram
'		
'		this.playReturned = true
'		
'		this.apu.apu_reset()
'		this.cpu._reset()
'		this.mapper->resetmapper_nsf()
'		
'		
'		this.frameirqwanted = false
'		'this.dmcirqwanted = false
'		
'	dim i as uint16_t = &H4000	
'	while i <=  &H4013
'		 this.apu.write_apu(i,0)
'		 i+=1
'	Wend
'    
'	 this.apu.write_apu(&H4015, 0) 
'    this.apu.write_apu(&H4015, &Hf) 
'    this.apu.write_apu(&H4017, &H40) 
'   
'   '////////////////////////////////////////////
'   
'    this.cpu.pc = &H3ff0
'    this.cpu.a = songNum - 1
'    this.cpu.x = 0
'
'
'		dim cycleCount As uint32_t = 0
'		Dim finished As boolean = FALSE
'		
'		 'while(cycleCount < 297800)
'		 '	
'		 'this.cpu._clock()
'		 '	
'		 	
'		 	
' '     '  this.apu.apu_cycle()
'		 	    
'		 	    
'		 	 if(this.cpu.pc = &H3ff5) then
' ''    ' '  // we are in the nops after the init-routine, it finished
' '    finished = TRUE 
' '   Exit While 
'		 	 End If
'		 
'      'cycleCount+=1
'' Wend
'		 
'		 
'   ' if(finished) = FALSE  Then
'   '  Print "Init did not finish within 10 frames" 
'   'End if
'	'	 	
'	
'End Sub

Sub NSFplayer.playsong(songNum As uint8_t)
		
		'reset////////////////////////////////
		Erase this.ram
		
		this.playReturned = true
		
		this.apu.apu_reset()
		this.cpu._reset()
		this.mapper->resetmapper_nsf()
		
		
		this.frameirqwanted = false
		'this.dmcirqwanted = false
		
	dim i as uint16_t = &H4000	
	while i <=  &H4013
		 this.apu.write_apu(i,0)
		 i+=1
	Wend
    
	 this.apu.write_apu(&H4015, 0) 
    this.apu.write_apu(&H4015, &Hf) 
    this.apu.write_apu(&H4017, &H40) 
   
   '////////////////////////////////////////////
   
    this.cpu.pc = &H3ff0
    this.cpu.a = songNum - 1
    this.cpu.x = 0


		dim cycleCount As uint32_t = 0
		Dim finished As boolean = FALSE
		
		 while(cycleCount < 297800)
		 	
		
		 	

		  this.cpu._clock() 
        this.apu.apu_cycle()
		 	    
    if(this.cpu.pc = &H3ff5) then
    ' ' '  // we are in the nops after the init-routine, it finished
    finished = TRUE 
    Exit While 
		End If 
	
		 	
		 
      cycleCount+=1
 Wend
		 
		 
    if(finished) = FALSE  Then
     Print "Init did not finish within 10 frames" 
   End if
		 	
	
End Sub
'FINISHED for now




''FINISHED for now
'Sub NSFplayer.runframe()
'	
'	If this.playreturned Then
'		this.cpu.pc = &H3FF8
'		
'	EndIf
'	this.playReturned = false
'	Dim cyclecount As uint32_t = 0
'	
'	
'	'while(cycleCount < 29780)
'		'
'	  ' this.cpu.irqWanted = this.dmcIrqWanted || this.frameIrqWanted;
'	   
'	   'this.cpu.irqWanted = this.dmcIrqWanted or this.frameIrqWanted 
'     do
'     	
'	   'this.cpu.irqWanted = this.frameIrqWanted 
'	   
'      if(this.playReturned = false) then
'        this.cpu._clock
'      end if
'     ' this.apu.apu_cycle() 
'      if(this.cpu.pc = &H3ffD) then
'       ' // we are in the nops after the play-routine, it finished
'        this.playReturned = true 
'      end if
'      
'      loop while this.cpu.complete() = false
'		  cycleCount+=1
'	'Wend
'End Sub

Sub NSFplayer.runframe()
	
	If this.playreturned Then
		this.cpu.pc = &H3FF8
		
	EndIf
	this.playReturned = false
	Dim cyclecount As uint32_t = 0
	
	
	while(cycleCount < 29780)
		'
	  ' this.cpu.irqWanted = this.dmcIrqWanted || this.frameIrqWanted;
	   
	   'this.cpu.irqWanted = this.dmcIrqWanted or this.frameIrqWanted 
   '  do
     	
	   'this.cpu.irqWanted = this.frameIrqWanted 
	   
      if(this.playReturned = false) then
        this.cpu._clock
      end if
     this.apu.apu_cycle() 
      if(this.cpu.pc = &H3ffD) then
       ' // we are in the nops after the play-routine, it finished
        this.playReturned = true 
      end if
      
    '  loop while this.cpu.complete() = false
		  cycleCount+=1
	Wend
End Sub

'finished for now - NOT SURE
Sub NSFplayer.getsamples(bytes_data() As double, count As uint16_t) 
    '// apu returns 29780 or 29781 samples (0 - 1) for a frame
   ' // we need count values (0 - 1)
   ' dim samples As outputs ptr = this.apu.getOutput() 
  this.APU.getOutput()
    dim runAdd As long= (29780 / count) 
    dim total As double =  0
    dim inputPos As long = 0 
    dim running As long  = 0 
    for i As uint16_t = 0 To count -1  
      running += runAdd 
      total = 0 
      Dim avgCount As uint16_t = running And &Hffff 
      
      for  j As uint16_t = inputPos  To(inputPos + avgCount) 
      total += this.apu._output(j) 'samples->apu_output(j)
       'cast(double,samples)[j]
      Next
      'rnd * 2-1*0.15
      'if bytes_data <> null then
      bytes_data(i) = total / avgCount 'this.apu._output(1)'total '/ avgCount 
      'end if
      '0.0028454054054054055 '
      inputPos += avgCount 
      running -= avgCount 
    Next
End Sub


'finished
 function  NSFplayer._write(adr as uint16_t, value as uint8_t) as boolean
	
	adr And= &Hffff 

    if(adr < &H2000) Then
    ' // ram
      this.ram(adr and &H7ff) = value 
      Return FALSE 
    End If
    if(adr < &H4000)  Then
    '  // ppu ports, not writable in NSF
     Return FALSE 
    End If
    if(adr < &H4020)  Then
     '// apu/misc ports
      if(adr = &H4014 or adr =  &H4016)  Then
        '// not writable in NSF
      
         Return FALSE  
          
      End If
       this.apu.write_apu(adr,value)

   
      Return FALSE 
     end if

  
 this.mapper->write_NSF(adr, value) 
	
	
End function

'finished
function NSFplayer._read(adr as uint16_t,bReadOnly as boolean) as uint8_t

    adr and= &Hffff 

    if(adr < &H2000) Then
    '  // ram
     return ram(adr And &H7ff)
    End If
    if(adr < &H3ff0) then
     ' // ppu ports, not readable in NSF
      return 0 
    End If
    if(adr < &H4000)  Then
     ' // special call area used internally by player
     Return this.callarea(adr and &Hf) 
    End If
    if(adr < &H4020)  Then
     ' // apu/misc ports
      if(adr =  &H4014)  Then
        return 0 '// not readable
      End If
      if(adr =  &H4016 or adr =  &H4017) Then
        return 0 '  // not readable in NSF
      End If
    return this.apu.read_apu(adr) 
    End If
    
   return this.mapper->read_NSF(adr) 
	
End function



































'type _CPU as CPU
'
'
'
'Type NSFplayer extends object
'	
'
'	   cpu as _CPU ptr
'	
'	ram(&H800) As uint8_t
''	Dim totalsongs As uint8_t
'	'Dim startsong  As uint8_t
'	
'	playreturned As boolean = TRUE
'	frameirqwanted As BOOLEAN =  FALSE
'	'Dim dmcirqwanted As boolean = FALSE
'	callarea(&H10) As uint8_t
'
'	NSFfile As NSFheader
'	
'	mapper As NSFmapper Ptr 
'	
'	
'	tag as tags
'	
'	Declare Constructor()
'	Declare Function LoadNSFile(NSF As String) As BOOLEAN
'	Declare Sub playsong(songnum1 As uint8_t)
'	Declare Function _read(adr As uint16_t,rdonly as boolean= false) As uint16_t
'	Declare Function _write(adr As uint16_t,value As uint8_t) As boolean
'	
'	Declare Sub getsamples(bytes_data As uint16_t ptr,count As uint16_t )
'	Declare Sub runframe()
'	
'	nes_apu as APU ptr
'	
'
'	
'End Type
'
'
'
'
'
'
'
''FINISHED for now
'Constructor NSFplayer()
'
'
'this.nes_apu = new APU(@this)
'
'Erase ram
'
'playreturned = TRUE
'frameirqwanted = FALSE
''dmcirqwanted = FALSE
'
'
'
'
'
'
'
'
'End Constructor
'
'
'
''FINISHED for now
'Sub NSFplayer.runframe()
'	
'	If playreturned Then
'		
'		'TODO WIP
'		
'	EndIf
'	this.playReturned = false
'	Dim cyclecount As uint32_t = 0
'	
'	
'	'while(cycleCount < 29780)
'	'	'
'	'	'      this.cpu.irqWanted = this.dmcIrqWanted || this.frameIrqWanted;
'   '   'if(!this.playReturned) {
'   '   '  this.cpu.cycle();
'   '   '}
'   '   this.nes_apu->apu_cycle() 
'   '   'if(this.cpu.br[0] === 0x3ffd) {
'   '   '  // we are in the nops after the play-routine, it finished
'   '   '  this.playReturned = true;
'   '   '}
'	'	  cycleCount+=1
'	'Wend
'	'
'	
'	
'End Sub
'
'
'
'
''FINISHED for now
'Sub NSFplayer.playsong(songNum As uint8_t)
'		
'		Erase ram
'		
'		this.playReturned = true
'		
'		this.nes_apu->apu_reset()
'		this.cpu->_reset()
'		this.mapper->resetmapper_nsf()
'		
'		
'		this.frameirqwanted = false
'		'this.dmcirqwanted = false
'		
'	for i As uint16_t = &H4000  to &H4013 
'      this.nes_apu->write_apu(i,0)
'   next 
'
'	 this.nes_apu->write_apu(&H4015, 0) 
'    this.nes_apu->write_apu(&H4015, &Hf) 
'    this.nes_apu->write_apu(&H4017, &H40) 
'   
'    this.cpu->pc = &H3ff0
'    this.cpu->a = songNum - 1
'    this.cpu->x = 0
'    
'   
'   
'    'this.cpu.br[0] = 0x3ff0;
'   ' this.cpu.r[0] = songNum - 1;
'    'this.cpu.r[1] = 0;
'		
'		
'		
'		dim cycleCount As uint32_t = 0
'		Dim finished As boolean = FALSE
'		
'		 while(cycleCount < 297800)
'		 	
'		 	  '    this.cpu.cycle();
'             this.nes_apu->apu_cycle()
'		 	    this.cpu->clock()
'		 	 if(this.cpu->pc = &H3ff5) then
'     ' '  // we are in the nops after the init-routine, it finished
'     finished = TRUE 
'     Exit While 
'    End If
'		 
'      cycleCount+=1
'		 Wend
'		 
'		 
'    if(finished) = FALSE  Then
'     Print "Init did not finish within 10 frames" 
'   End if
'		 	
'	
'End Sub
'	
'	
'
''finished for now
'Sub NSFplayer.getsamples(bytes_data As uint16_t Ptr, count As uint16_t) 
'    '// apu returns 29780 or 29781 samples (0 - 1) for a frame
'   ' // we need count values (0 - 1)
'    dim samples As outputs Ptr = this.nes_apu->getOutput() 
'    dim runAdd As uint16_t = (29780 / count) 
'    dim total As uint16_t =  0
'    dim inputPos As uint16_t  = 0 
'    dim running As uint16_t  = 0 
'    for i As uint16_t = 0 To count -1  
'      running += runAdd 
'      total = 0 
'      Dim avgCount As uint16_t = running And &Hffff 
'      for  j As uint16_t = inputPos  To(inputPos + avgCount) 
'     total += samples->apu_output[j] 
'      Next
'      
'      if bytes_data <> null then
'      bytes_data[i]  = total / avgCount 
'      end if
'      
'      inputPos += avgCount 
'      running -= avgCount 
'    Next
'End Sub
'	
'	'FINISHED
'	function NSFplayer._read(adr As uint16_t,rdonly as boolean= false) As uint16_t
'
'    adr and= &Hffff 
'
'    if(adr < &H2000) Then
'    '  // ram
'      return ram(adr And &H7ff)
'    End If
'    if(adr < &H3ff0) then
'     ' // ppu ports, not readable in NSF
'      return 0 
'    End If
'    if(adr < &H4000)  Then
'     ' // special call area used internally by player
'     Return this.callarea(adr and &Hf) 
'    End If
'    if(adr < &H4020)  Then
'     ' // apu/misc ports
'      if(adr =  &H4014)  Then
'        return 0 '// not readable
'      End If
'      if(adr =  &H4016 or adr =  &H4017) Then
'        return 0 '  // not readable in NSF
'      End If
'     return this.nes_apu->read_apu(adr) 
'    End If
'    return this.mapper->read_NSF(adr) 
'
'End Function
'
'
''FINISHED
'Function NSFplayer._write(adr As uint16_t,value As uint8_t) As boolean
'		
'		
'		  adr And= &Hffff 
'
'    if(adr < &H2000) Then
'    ' // ram
'      this.ram(adr and &H7ff) = value 
'      Return FALSE 
'    End If
'    if(adr < &H4000)  Then
'    '  // ppu ports, not writable in NSF
'     Return FALSE 
'    End If
'    if(adr < &H4020)  Then
'     '// apu/misc ports
'      if(adr = &H4014 or adr =  &H4016)  Then
'        '// not writable in NSF
'       Return FALSE  
'      End If
'      
'    this.nes_apu->write_apu(adr,value)
'    
'      Return FALSE 
' 
'    this.mapper->write_NSF(adr, value) 
'  End If
'
'	
'	
'End Function
'	

#include "olc6502.bas"
#include "APU.bas"	
	
	
	
