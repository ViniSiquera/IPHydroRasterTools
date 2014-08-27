'*******************************************************************
'DETERMINAÇÃO DE BACIAS
'Criado por Vinícius Alencar Siqueira - 20/01/2014
'*******************************************************************
Imports IPHDataManagement

''' <summary>
''' Classe para identificação de bacias hidrográficas a partir de exutórios sobre a a rede de drenagem.
''' </summary>
''' <remarks></remarks>
Public Class Catchments

    Private _WaterShed As RasterInteger 'Raster com os dados das bacias
    Private _FlowDirection As RasterInteger 'Raster com FlowDirection
    Private _CellExutorios() As CellWatershed 'Células com a posição dos exutórios

#Region "Métodos e funções"
    ''' <summary>
    ''' Retorna a matriz de Catchments
    ''' </summary>
    Public Function GetWaterShedData() As RasterInteger
        Return _WaterShed.CopyData
    End Function

    ''' <summary>
    ''' Retorna o Flow Direction
    ''' </summary>
    Public Function GetFlowDirectionData() As RasterInteger
        Return _FlowDirection.CopyData
    End Function

    Public Sub SetFlowDirectionData(ByVal FD As RasterInteger)
        _FlowDirection = FD
        _WaterShed = New RasterInteger(_FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.XllCorner, _FlowDirection.YllCorner, _FlowDirection.Cellsize, _FlowDirection.NoDataValue)
    End Sub
#End Region

    ''' <summary>
    ''' Retorna as células com a posição dos exutórios
    ''' </summary>
    Public ReadOnly Property CelulasExutorios As CellWatershed()
        Get
            Return _CellExutorios
        End Get
    End Property

    ''' <summary>
    ''' Cria uma nova instância da class
    ''' </summary>
    Public Sub New()
    End Sub

    'Lê os dados do FlowDirection
    Public Sub ReadFlowDirectionData(ByVal local As String)
        _FlowDirection = New RasterInteger
        _FlowDirection.ReadData(local)
        _WaterShed = New RasterInteger(_FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.XllCorner, _FlowDirection.YllCorner, _FlowDirection.Cellsize, _FlowDirection.NoDataValue)
    End Sub

    'Lê os dados do StreamSegmentation
    Public Sub ReadStreamSegmentData(ByVal local As String)
        _WaterShed.ReadData(local) 'Pega os dados de stream segment e joga na matriz de watershed para marcar pontos na bacia
    End Sub

    ''' <summary>
    ''' Atribui os exutóros na matriz
    ''' </summary>
    ''' <param name="ShapeFileOutlets">Descrição: O arquivo em formato shapefile com os exutorios</param>
    Public Sub SetPointOutlets(ByVal ShapeFileOutlets As String)

        Dim sf As New MapWinGIS.Shapefile 'Shapefile com os exutórios
        Dim sh As New MapWinGIS.Shape

        sf.Open(ShapeFileOutlets) 'Abre o arquivo shapefile dos exutórios
        Dim nCellExutorios As Integer = sf.NumShapes 'atribui o número de exutórios

        ReDim _CellExutorios(nCellExutorios - 1) 'Redimensiona a variavel que armazena os exutórios

        For i = 0 To nCellExutorios - 1
            'Now get the current shape at position i (polygon or polyline) 
            sh = sf.Shape(i)
            '_CellExutorios(i) = New CellWatershed(sh.Point(0).y, sh.Point(0).x, _FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.Cellsize, _FlowDirection.XllCorner, _FlowDirection.YllCorner, i)
            _CellExutorios(i) = New CellWatershed(sh.Point(0).y, sh.Point(0).x, _FlowDirection.Linhas, _FlowDirection.Colunas, _FlowDirection.Cellsize, _FlowDirection.XllCorner, _FlowDirection.YllCorner, i + 1)
        Next

        InsertOutlets() 'Insere os exutórios na matriz de dados

    End Sub

    ''' <summary>
    ''' Identifica as catchments
    ''' </summary>
    Public Sub FindWatersheds()

        'If _CellExutorios Is Nothing Then Throw New Exception("Não existem pontos de exutório!")

        IdentificaWatershed()

    End Sub

    'Identifica a bacia hidrográfica
    Private Sub IdentificaWatershed()

        Dim x, xAnterior, xInicial As Integer
        Dim y, yAnterior, yInicial As Integer

        For i = 0 To _FlowDirection.Linhas - 1
            y = i
            For j = 0 To _FlowDirection.Colunas - 1
                x = j

                yInicial = y : xInicial = x 'Identificar a célula de partida para atualizar a matriz Watershed

                Do
                    xAnterior = x : yAnterior = y 'Identifica a posição da célula antes de mover-se na matriz
                    If _FlowDirection.Dados(y, x) = _FlowDirection.NoDataValue Then
                        AtualizaWaterShed(yInicial, xInicial, _FlowDirection.NoDataValue) : Exit Do
                    End If

                    FlowDirection.MoveToFlowDirection(_FlowDirection.Dados(y, x), y, x) 'Identifica a célula seguinte, para onde está direcionado o fluxo

                    If x < 0 OrElse y < 0 OrElse x > (_FlowDirection.Colunas - 1) OrElse y > (_FlowDirection.Linhas - 1) Then 'Cheguei na borda do MDE
                        AtualizaWaterShed(yInicial, xInicial, _FlowDirection.NoDataValue) : Exit Do

                    ElseIf x = xAnterior And y = yAnterior Then 'Cheguei em um NODATA; o caminho ao longo da matriz deverá ser = NODATA
                        AtualizaWaterShed(yInicial, xInicial, _FlowDirection.NoDataValue) : Exit Do

                    ElseIf _WaterShed.Dados(y, x) > 0 Then 'Cheguei em um dos exutórios
                        AtualizaWaterShed(yInicial, xInicial, _WaterShed.Dados(y, x)) : Exit Do
                    End If

                    'Se encontrar um noData pelo caminho é sinal de que este caminho não dará em lugar algum
                    If _WaterShed.Dados(y, x) = _FlowDirection.NoDataValue Then
                        AtualizaWaterShed(yInicial, xInicial, _FlowDirection.NoDataValue) : Exit Do
                    End If

                    'Se o atributo for igual a 0 na matriz watershed, continua seguindo através das direções de fluxo
                Loop
                y = i 'retorna o y para a linha atual
            Next
        Next

    End Sub

    'Insere os pontos dos exutórios especificados pelo shapefile de entrada
    Private Sub InsertOutlets()

        'Insere em cada exutório o valor do atributo, que corresponderá à Sub-bacia
        For i = 0 To _CellExutorios.Count - 1
            _WaterShed.Dados(_CellExutorios(i).y, _CellExutorios(i).x) = _CellExutorios(i).Atributo
        Next

    End Sub

    'Insere o valor do atributo na linha e coluna especificada, na matriz watershed
    Private Sub AtualizaWaterShed(ByVal linha As Integer, ByVal coluna As Integer, ByRef Atributo As Int16)

        '0 = célula ainda não identificada

        Dim lin As Integer = linha
        Dim col As Integer = coluna

        Do 'Enquanto nada tiver sido verificado
            If _WaterShed.Dados(lin, col) = 0 Then
                _WaterShed.Dados(lin, col) = Atributo

                FlowDirection.MoveToFlowDirection(_FlowDirection.Dados(lin, col), lin, col)
                If col < 0 OrElse lin < 0 OrElse col > (_FlowDirection.Colunas - 1) OrElse lin > (_FlowDirection.Linhas - 1) Then Exit Do 'Cheguei na borda do MDE

            Else
                Exit Do 'Já identifiquei esta célula com algum identificador, não preciso verificar denovo o caminho 
            End If
        Loop

    End Sub

    'Escreve o arquivo das bacias 
    Public Sub WriteWaterShedData(ByVal Arquivo As String)
        _WaterShed.WriteData(Arquivo)
    End Sub


End Class
