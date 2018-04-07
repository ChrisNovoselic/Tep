Attribute VB_Name = "NAMED_CELLS"

Sub ИменаДляЯчеек()
Attribute ИменаДляЯчеек.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Макрос2 Макрос
'

'
    Dim nameCell

    Dim numSheet, nameSheet, numPar, rowPar, colPar
    'numSheet = 1
    'nameSheet = "Общие_показатели_электростанции"
    
    'numSheet = 2
    'nameSheet = "Показатели_турбоагрегатов"
    
    numSheet = 3
    nameSheet = "Показатели_котлоагрегатов"
    
    'numSheet = 4
    'nameSheet = "Потери_Отклонение"

    indxPar1 = 28
    rowPar = 43
    
    For indxPar2 = 1 To 14
        'For j = 1 To 2
            colPar = indxPar2 + (6 - 1)
            Cells(rowPar, colPar).Select
'            If indxPar > 6 * 2 Then
'                nameCell = "A" + Trim(Str(numSheet)) + "_" + Trim(Str(indxPar)) + "_S"
'            Else
'                nameCell = "A" + Trim(Str(numSheet)) + "_" + Trim(Str(indxPar)) + "_" + Trim(Str(numBlock))
'            End If

            nameCell = "A" + Trim(Str(numSheet)) + "_" + Trim(Str(indxPar1)) + "_" + Trim(Str(indxPar2))

'            If (indxPar Mod 2) = 1 Then
'                nameCell = nameCell + "_N"
'            Else
'                nameCell = nameCell + "_F"
'            End If
            ActiveWorkbook.Worksheets(nameSheet).Names.Add Name:=nameCell, RefersToR1C1:="=" + nameSheet + "!R" + Trim(Str(rowPar)) + "C" + Trim(Str(colPar))
            ActiveWorkbook.Worksheets(nameSheet).Names(nameCell).Comment = ""
        'Next j
    Next indxPar2
End Sub
