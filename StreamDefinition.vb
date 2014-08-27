'*******************************************************************
'IDENTIFICAÇÃO DE TRECHOS DE RIO
'Criado por Vinícius Alencar Siqueira - 20/01/2014
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Classe utilizada para definir trechos de rio a partir de área acumulada
''' </summary>
Public Class StreamDefinition

    Private _FlowAcc As RasterReal 'Matriz de Fluxo acumulado
    Private _StreamDef As RasterInteger 'Matriz com o Stream Definition
    Private _StreamGroups As RasterReal 'Matriz com os Grupos de treshold
    Private _Threshold As Long 'Número mínimo de células acumuladas para formar uma rede de drenagem

    ''' <summary>
    ''' Identifica o tipo de entrada das informações
    ''' </summary>
    Public Enum ThresholdType

        NumberOfCells = 0
        PercentualOfMaximumCells = 1
        Area = 2 'km²

    End Enum

#Region "Métodos e funções"
    ''' <summary>
    ''' Retorna o valor do Threshold a ser utilizado
    ''' </summary>
    Public ReadOnly Property ThresholdValue As Long
        Get
            Return _Threshold
        End Get
    End Property

    ''' <summary>
    ''' Retorna o o Stream Definition 
    ''' </summary>
    Public Function GetStreamDefinitionData() As RasterInteger
        Return _StreamDef.CopyData
    End Function
    ''' <summary>
    ''' Retorna o o Stream Groups 
    ''' </summary>
    Public Function GetStreamGroupsData() As RasterReal
        Return _StreamGroups
    End Function
    ''' <summary>
    ''' Retorna a matriz de Flow Acumulation
    ''' </summary>
    Public Function GetFlowAccumulatioData() As RasterReal
        Return _FlowAcc.CopyData
    End Function

#End Region

    ''' <summary>
    ''' Cria um novo arquivo de StreamDefinition
    ''' </summary>
    Public Sub New()
        Me._FlowAcc = New RasterReal
    End Sub
    ''' <summary>
    ''' Lê um arquivo de Flow accumulation, especificando o local
    ''' </summary>
    Public Sub ReadFlowAccumulation(ByVal Local As String, ByVal thresoldValue As Single, ByVal thresType As ThresholdType)

        Me._FlowAcc.ReadData(Local)
        CalculateThreshold(thresoldValue, thresType)
        _StreamDef = New RasterInteger(_FlowAcc.Linhas, _FlowAcc.Colunas, _FlowAcc.XllCorner, _FlowAcc.YllCorner, _FlowAcc.Cellsize, _FlowAcc.NoDataValue)

    End Sub

    ''' <summary>
    ''' Lê um arquivo de grupos, com thresholds diferentes para diferentes areas (opcional)
    ''' </summary>
    Public Sub ReadStreamGroupsData(ByVal Local As String)

        Me._StreamGroups = New RasterReal
        Me._StreamGroups.ReadData(Local)

    End Sub

    'Calcula o valor do threshold a ser adotado
    Private Sub CalculateThreshold(ByVal value As Single, ByVal thrType As ThresholdType)

        If thrType = ThresholdType.PercentualOfMaximumCells Then
            _Threshold = CLng(_FlowAcc.MaxValue * value) 'Multiplico o máximo nº de células acumuladas por um percentual
        ElseIf thrType = ThresholdType.NumberOfCells Then
            _Threshold = CLng(value) ' Retorna o próprio valor
        ElseIf thrType = ThresholdType.Area Then
            _Threshold = CLng(value / ((_FlowAcc.Cellsize * 111) ^ 2)) 'Considera que 1º tenha aprox. 111km
        End If

    End Sub

    ''' <summary>
    ''' Rotina que define os trechos de rio.
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub DefineStreams()

        If _Threshold < 1 Then Throw New ArgumentException("O número mínimo de células para formar uma rede de drenagem é insuficiente.")
        If _StreamGroups Is Nothing Then SetUniqueTreshold() 'Se o arquivo de grupos não for carregado, atribui um grupo único

        For nRow = 0 To _StreamDef.Linhas - 1
            For nCol = 0 To _StreamDef.Colunas - 1

                If _FlowAcc.Dados(nRow, nCol) = _FlowAcc.NoDataValue Then 'Se encontrar NODATA escreve NOData
                    _StreamDef.Dados(nRow, nCol) = _FlowAcc.NoDataValue
                ElseIf _FlowAcc.Dados(nRow, nCol) >= _StreamGroups.Dados(nRow, nCol) Then 'Se for maior do que o threshold, marca a célula
                    _StreamDef.Dados(nRow, nCol) = 1
                Else 'Se for inferior ao threshold, não marca a célula
                    _StreamDef.Dados(nRow, nCol) = 0
                End If

            Next
        Next

    End Sub

    'Atribui um único grupo com treshold defiido
    Private Sub SetUniqueTreshold()

        'Cria um novo Raster de Grupos, sendo que este será para um grupo de threshold único
        _StreamGroups = New RasterReal(_FlowAcc.Linhas, _FlowAcc.Colunas, _FlowAcc.XllCorner, _FlowAcc.YllCorner, _FlowAcc.Cellsize, _FlowAcc.NoDataValue)

        For nRow = 0 To _StreamDef.Linhas - 1
            For nCol = 0 To _StreamDef.Colunas - 1
                _StreamGroups.Dados(nRow, nCol) = _Threshold
            Next
        Next

    End Sub

    'Escreve os dados em asc
    Public Sub WriteStrDefinitionData(ByVal arquivo As String)
        Me._StreamDef.WriteData(arquivo)
    End Sub

End Class
