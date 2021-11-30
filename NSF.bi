







'#pragma once
'
'#include "crt.bi"
'
'#include "olc6502.bi"
'#include "olc6502.bas"
'
'#include "olc2A02.bas"
'

'
'
'
''print "in Bus.bi"
'
'type tags
'	name as string
'	copyright as String
'	artist as String
'End Type
'
'
'
'type _APU as APU
'
'Type Bus
'	
'	cpu as olc6502
'	apu as _APU
'	
'	
'	
'	
'	
'
'	static ram(&H800) As uint8_t
'	
'	callarea(&H10) As uint8_t
'	
'	'NSFfile As NSFheader
'	
'	'mapper As NSFmapper Ptr 
'	
'	Dim totalsongs As uint8_t
'	Dim startsong  As uint8_t
'	tag as tags
'	
'	playreturned As boolean = TRUE
'	frameirqwanted As BOOLEAN =  FALSE
'	'Dim dmcirqwanted As boolean = FALSE
'	
'	Declare Constructor()
'	
'	Declare Function LoadNSFile(NSF As String) As BOOLEAN
'	Declare Sub playsong(songnum1 As uint8_t)
'	Declare Sub runframe()
'	Declare Sub getsamples(bytes_data As uint16_t ptr,count As uint16_t )	
'	Declare Function _read(adr As uint16_t,rdonly as boolean= false) As uint16_t
'	Declare Function _write(adr As uint16_t,value As uint8_t) As boolean
'	
'	'nes_apu as apu ptr
'	
'
'	
'End Type



type as NSFplayer _Bus

#pragma once

#include "crt.bi"

#include "olc6502.bi"
#include "APU.bi"
'#include "olc2A02.bi"

#Include "NSFmapper.bas"


'print "in Bus.bi"





type tags
	namesong as string
	copyright as String
	artist as String
End Type



Type NSFheader
	
	format As  String * 5 
	version As uint8_t 
	totalsongs As uint8_t
	startsong As uint8_t
	loadaddr As uint32_t
	initaddr As uint16_t
	playaddr As uint16_t
	Namesong As String '*32
	artist As String '*32
	copyright As String '*32
	initbanks(7) As uint8_t
	total As uint8_t  
	banking As boolean
	con_progdata As uint8_t ptr = New uint8_t(3)
	'romdata As uint8_t = New uint8_t(Any)
	rom_data As TVECTORUINT8T 
	
	' need to know what this is for...
	
	
	
	
	
	
	
End Type



type NSFplayer
	
	public:
	
	declare constructor()
	
	
		
	ram(&H800) As uint8_t
	
   callarea(&H10) As uint8_t

   mapper As NSFmapper Ptr 
   
   NSFfile As NSFheader
  
  
  'devices on bus
	cpu as olc6502
	apu as APU
	
	'////////////
	
	
	'apu as olc2A02
	'ppu as olc2C02
	 
   Dim totalsongs As uint8_t
	Dim startsong  As uint8_t
	tag as tags
	
	playreturned As boolean = TRUE
	frameirqwanted As BOOLEAN =  FALSE
	 	 
 	'dmcIrqWanted as boolean = false
 	 

	Declare Function LoadNSFile(NSF As String,byref disasm as TMAPUINT16TSTRING) As BOOLEAN
	Declare Sub playsong(songnum1 As uint8_t)
	Declare Sub runframe()
	Declare Sub getsamples(bytes_data() As double,count As uint16_t )	
	Declare Function _read(adr As uint16_t,rdonly as boolean= false) As uint8_t
	Declare Function _write(adr As uint16_t,value As uint8_t) As boolean

	
End Type

'dim NSFplayer.ram(64 * 1024) as uint8_t

