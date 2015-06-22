'*******************************************************************
'DIREÇÕES DE FLUXO (com base na codificação ArcHydro)
'Criado por Vinícius Alencar Siqueira - 20/01/2014
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Calcula as direções de fluxo a partir de um MDE sem depressões
''' </summary>
''' <remarks></remarks>
Public Class FlowDirection

    Private _MDE, _FlowDirection As RasterInteger 'Rasters
    Private _MatrizD8(8) As Single 'Matriz dos 8 vizinhos
    Private _CountDepressions As Long

#Region "Métodos e funções"
    ''' <summary>
    ''' Retorna o MDE original ou modificado pelo processo
    ''' </summary>
    Public Function GetMDEdata() As RasterInteger
        Return _MDE.CopyData
    End Function

    ''' <summary>
    ''' Retorna o Flow Direction
    ''' </summary>
    Public Function GetFlowDirectionData() As RasterInteger
        Return _FlowDirection.CopyData
    End Function

#End Region

    Public Sub New()
        Me._MDE = New RasterInteger
    End Sub

    'Lê um arquivo de MDE, especificando o local
    Public Sub ReadMDEdata(ByVal Local As String)

        Me._MDE.ReadData(Local)
        Me._FlowDirection = New RasterInteger(_MDE.Linhas, _MDE.Colunas, _MDE.XllCorner, _MDE.YllCorner, _MDE.Cellsize, _MDE.NoDataValue)

    End Sub

    'Identifica as direções de fluxo e as depressões espúrias do MDE
    Public Sub CalcularFlowDirection()

        Dim FDFound As Boolean
        Dim countDepression As Long = 0

        'Calcula para toda a grade as direções de fluxo
        For y = 1 To Me._MDE.Linhas - 2
            For x = 1 To Me._MDE.Colunas - 2
                _FlowDirection.Dados(y, x) = IncipientFlowDirection(y, x)
                If _FlowDirection.Dados(y, x) = 0 Then 'Se for = 0, verifica se existe uma célula com igual cota para onde possa verter 

                    VerifyFlowDirAtBounds(y, x, FDFound)

                    If FDFound = False Then ' Se não encontrar célula para verter água, identifica uma depressão
                        _CountDepressions += 1
                    End If

                    FDFound = False

                End If
            Next
        Next

    End Sub

    'Verifica se algum vizinho de mesma cota, ou inferior já possui flow direction, atribuindo a mesma em caso verdadeiro
    Private Sub VerifyFlowDirAtBounds(ByVal Yc As Int16, ByVal Xc As Int16, ByRef Validated As Boolean)

        'na openlist:,         'direções de apontamento para a célula central
        'For y = -1 To 1       2  4  8
        'posY = (Yc + y)       1    x   16
        'For x = -1 To 1       128   64  32
        'posX = (xc + x)

        Validated = False
        Dim elevation As Int16 = _MDE.Dados(Yc, Xc)
        Dim yi, xi As Int16

        For j = -1 To 1
            yi = Yc + j
            For i = -1 To 1
                xi = Xc + i

                If i <> 0 OrElse j <> 0 Then 'Exclui a célula central

                    If _MDE.Dados(yi, xi) = elevation Then 'Somente se a célula possui cota igual àquela sendo analisada
                        If _FlowDirection.Dados(yi, xi) = 0 Then Validated = True

                        If Validated = True Then 'caso tenha sido encontrado algum vizinho com direção de fluxo atribuída, manda água pra ele
                            _FlowDirection.Dados(Yc, Xc) = RelativeIncipientFlowDirection(Yc, yi, Xc, xi)
                            GoTo endVerify
                        End If

                    End If
                End If
            Next
        Next
endVerify:

    End Sub

    'Calcula a Direção de fluxo nos limites da grade
    Private Sub FlowDirectionAtBounds()

        'Define o Flow direction dos cantos
        _FlowDirection.Dados(0, 0) = 32
        _FlowDirection.Dados(0, _MDE.Colunas - 1) = 128
        _FlowDirection.Dados(_MDE.Linhas - 1, 0) = 8
        _FlowDirection.Dados(_MDE.Linhas - 1, _MDE.Colunas - 1) = 2

        'Atribui o flow direction para fora dos limites da grade
        For y = 1 To _MDE.Linhas - 2
            _FlowDirection.Dados(y, 0) = 16
        Next

        For y = 1 To _MDE.Linhas - 2
            _FlowDirection.Dados(y, _MDE.Colunas - 1) = 1
        Next

        For x = 1 To _MDE.Colunas - 2
            _FlowDirection.Dados(0, x) = 64
        Next

        For x = 1 To _MDE.Colunas - 2
            _FlowDirection.Dados(_MDE.Linhas - 1, x) = 4
        Next

    End Sub

    'Retorna a direção de fluxo de uma célula qualquer, posicionada em x,y; Função das declividades das células vizinhas
    Private Function IncipientFlowDirection(ByVal y As Integer, ByVal x As Integer) As Int16

        Dim valor As Int16 = _MDE.Dados(y, x)
        If valor = _MDE.NoDataValue Then Return _MDE.NoDataValue

        'Matriz D8 - 8 vizinhos:
        '0  1  2
        '3  x  4
        '5  6  7

        'Calcula o valor da declividade para cada um dos vizinhos
        'Aquele que tiver a maior declividade é para onde irá a direção de fluxo
        'Para os sentidos 0, 2, 5 e 7 o comprimento é igual a Raiz(2)
        'Para os sentidos 1, 3, 4 e 6 o comprimento é igual a 1

        _MatrizD8(0) = (valor - _MDE.Dados(y - 1, x - 1)) / Math.Sqrt(2)
        _MatrizD8(1) = valor - _MDE.Dados(y - 1, x)
        _MatrizD8(2) = (valor - _MDE.Dados(y - 1, x + 1)) / Math.Sqrt(2)
        _MatrizD8(3) = valor - _MDE.Dados(y, x - 1)
        _MatrizD8(4) = valor - _MDE.Dados(y, x + 1)
        _MatrizD8(5) = (valor - _MDE.Dados(y + 1, x - 1)) / Math.Sqrt(2)
        _MatrizD8(6) = valor - _MDE.Dados(y + 1, x)
        _MatrizD8(7) = (valor - _MDE.Dados(y + 1, x + 1)) / Math.Sqrt(2)

        '32	64	    128
        '16	0	    1      'Configuração das direções de fluxo para o IDRISI Kilimanjaro
        '8	4	    2

        Dim max As Single = 0.01
        Dim enumMax As Int16 = -1 'Valor inicial que indica que não há direção de fluxo

        'Seleciona a maior declividade dentre as calculadas
        For i = 0 To 7
            If _MatrizD8(i) > max Then max = _MatrizD8(i) : enumMax = i
        Next

        Select Case enumMax  'Identifica o sentido conforme o número do enumerador máximo
            Case 0
                Return 32
            Case 1
                Return 64
            Case 2
                Return 128
            Case 3
                Return 16
            Case 4
                Return 1
            Case 5
                Return 8
            Case 6
                Return 4
            Case 7
                Return 2
            Case Else
                Return 0
        End Select

    End Function

    'Retorna o indicador da direção de fluxo comparando duas células adjacentes
    Protected Friend Shared Function RelativeIncipientFlowDirection(ByVal y1 As Int16, ByVal y2 As Int16, ByVal x1 As Int16, ByVal x2 As Int16) As Int16

        'célula 1 = célula de origem
        'célula 2 = célula de destino

        Dim xRel As Int16 = x2 - x1
        Dim yRel As Int16 = y2 - y1

        If xRel = 1 And yRel = -1 Then Return 128
        If xRel = 1 And yRel = 0 Then Return 1
        If xRel = 1 And yRel = 1 Then Return 2
        If xRel = 0 And yRel = 1 Then Return 4
        If xRel = -1 And yRel = 1 Then Return 8
        If xRel = -1 And yRel = 0 Then Return 16
        If xRel = -1 And yRel = -1 Then Return 32
        If xRel = 0 And yRel = -1 Then Return 64

        'Caso encontre algum outro valor dispara um exception
        Throw New Exception("Problemas ao tentar identificar a direção de fluxo do trecho de rio modificado.")

    End Function

    'Move a linha e a coluna de acordo com o número armazenado
    Protected Friend Shared Sub MoveToFlowDirection(ByVal valor As Int16, ByRef Lin As Int16, ByRef col As Int16)

        '32	64	    128
        '16	0	    1      'Configuração das direções de fluxo para o IDRISI Kilimanjaro
        '8	4	    2

        Select Case valor 'Identifica o sentido conforme o numero 

            Case 128
                Lin = Lin - 1
                col = col + 1
            Case 1
                col = col + 1
            Case 2
                Lin = Lin + 1
                col = col + 1
            Case 4
                Lin = Lin + 1
            Case 8
                Lin = Lin + 1
                col = col - 1
            Case 16
                col = col - 1
            Case 32
                Lin = Lin - 1
                col = col - 1
            Case 64
                Lin = Lin - 1
            Case Else
                'Não muda, pois encontrou uma borda/depressão
        End Select
    End Sub

    ''' <summary>
    ''' Escreve a matriz de dados do flow direction
    ''' </summary>
    Public Sub WriteFlowDirectionData(ByVal Arquivo As String)
        _FlowDirection.WriteData(Arquivo)
    End Sub

End Class
