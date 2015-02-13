'**************************************************************
'DETERMINAÇÃO DAS URHs COM BASE NO USO DO SOLO, COBERTURA VEGETAL E TIPO DE SOLO
'Criado por Vinícius Siqueira - 13/02/2015
'IPH - Instituto de Pesquisas Hidráulicas - UFRGS
'**************************************************************

Imports IPHDataManagement

''' <summary>
''' Classe que manipula os blocos (Geração de URHs)
''' </summary>
''' <remarks></remarks>
Public Class Blocks

    Private _BD As BlockDatabase
    Private _LandUseVeget As RasterInteger 'Raster do uso de solo e cobertura vegetal
    Private _SoilTypes As RasterInteger 'Raster do tipo de solo
    Private _HRUraster As RasterInteger 'Raster da composição
    Private _LandUseReclassParameters() As SpatialAnalyst.ReclassParameters
    Private _SoilTypeReclassParameters() As SpatialAnalyst.ReclassParameters

    Public Sub New()
        _BD = New BlockDatabase
    End Sub

    'Retorna a base de dados
    Public ReadOnly Property Database As BlockDatabase
        Get
            Return _BD
        End Get
    End Property
    'Retorna o raster de uso do solo
    Public ReadOnly Property LandUseRaster As RasterInteger
        Get
            Return _LandUseVeget
        End Get
    End Property
    'Retorna o raster de tipo de solo
    Public ReadOnly Property SoilType As RasterInteger
        Get
            Return _SoilTypes
        End Get
    End Property

    'Retorna o raster de tipo de solo
    Public ReadOnly Property HRUraster As RasterInteger
        Get
            Return _HRUraster
        End Get
    End Property

    'Identifica O número de classes do arquivo de uso do solo e cobertura vegetal
    Public Sub IdentifyLandUseNumberOfClasses()

        _BD.LandUseBlock.Clear()

        Dim classes As Integer = 1
        Dim Count(0) As Long

        For i = 0 To _LandUseVeget.Linhas - 1
            For j = 0 To _LandUseVeget.Colunas - 1

                If Not _LandUseVeget.Dados(i, j) = _LandUseVeget.NoDataValue Then 'Conta somente se não for NoDATA
                    If _LandUseVeget.Dados(i, j) > classes Then 'Se o valor encontrado for maior do que o número de classes
                        classes = _LandUseVeget.Dados(i, j) 'Assume o novo valor e redimensiona o vetor contador de células
                        ReDim Preserve Count(classes - 1)
                    End If
                    Count(_LandUseVeget.Dados(i, j) - 1) += 1
                End If

            Next
        Next

        'Adiciona os dados no database
        For z = 0 To Count.Count - 1
            _BD.LandUseBlock.Add(New BlockClasses)
            _BD.LandUseBlock.Item(z).SetRasterNumber(z + 1)
            _BD.LandUseBlock.Item(z).SetCountNumber(Count(z))
            _BD.LandUseBlock.Item(z).SetPercentNumber(Count(z) / Count.Sum)
        Next
    End Sub

    'Identifica O número de classes do arquivo de uso do solo e cobertura vegetal
    Public Sub IdentifySoilNumberOfClasses()

        _BD.SoilBlock.Clear()

        Dim classes As Integer = 1
        Dim Count(0) As Long

        For i = 0 To _SoilTypes.Linhas - 1
            For j = 0 To _SoilTypes.Colunas - 1

                If Not _SoilTypes.Dados(i, j) = _SoilTypes.NoDataValue Then 'Conta somente se não for NoDATA
                    If _SoilTypes.Dados(i, j) > classes Then 'Se o valor encontrado for maior do que o número de classes
                        classes = _SoilTypes.Dados(i, j) 'Assume o novo valor e redimensiona o vetor contador de células
                        ReDim Preserve Count(classes - 1)
                    End If
                    Count(_SoilTypes.Dados(i, j) - 1) += 1
                End If

            Next
        Next

        'Adiciona os dados no database
        For z = 0 To Count.Count - 1
            _BD.SoilBlock.Add(New BlockClasses)
            _BD.SoilBlock.Item(z).SetRasterNumber(z + 1)
            _BD.SoilBlock.Item(z).SetCountNumber(Count(z))
            _BD.SoilBlock.Item(z).SetPercentNumber(Count(z) / Count.Sum)
        Next

    End Sub

    'Lê os dados de Uso do solo e cobertura vegetal
    Public Sub ReadLandUseVegetationCoverData(ByVal filename As String)
        _LandUseVeget = New RasterInteger
        _LandUseVeget.ReadData(filename)
    End Sub
    'Lê os dados de tipo de solo
    Public Sub ReadSoilTypesData(ByVal filename As String)
        _SoilTypes = New RasterInteger
        _SoilTypes.ReadData(filename)
    End Sub
    'Escreve os dados de Uso do solo e cobertura vegetal
    Public Sub WriteLandUseVegetationCoverData(ByVal filename As String)
        _LandUseVeget.WriteData(filename)
    End Sub
    'Escreve os dados de tipo de solo
    Public Sub WriteSoilTypesData(ByVal filename As String)
        _SoilTypes.WriteData(filename)
    End Sub

    'Escreve o arquivo de texto com os nomes das classes
    'tipo = "Soil" ou "Landuse"
    Public Sub SaveClassNamesToFile(ByVal Filename As String, ByVal tipo As String)
        If tipo <> "Soil" AndAlso tipo <> "Landuse" Then Throw New ArgumentException("Class type " & tipo & " is not known.")

        Using text As New IO.StreamWriter(Filename)
            text.WriteLine("ID;Actual Class;Reclassified Class")

            Dim actualname, reclassifiedname As String

            Select Case tipo
                Case "Soil"
                    For i = 0 To _BD.SoilBlock.Count - 1
                        actualname = _BD.SoilBlock(i).ActualName : If actualname = "<Unclassified>" Then actualname = ""
                        reclassifiedname = _BD.SoilBlock(i).ReclassifiedName : If reclassifiedname = "<Unclassified>" Then reclassifiedname = ""

                        text.WriteLine(i + 1 & ";" & _BD.SoilBlock(i).ActualName & ";" & _BD.SoilBlock(i).ReclassifiedName)
                    Next
                Case "Landuse"
                    For i = 0 To _BD.LandUseBlock.Count - 1
                        actualname = _BD.LandUseBlock(i).ActualName : If actualname = "<Unclassified>" Then actualname = ""
                        reclassifiedname = _BD.LandUseBlock(i).ReclassifiedName : If reclassifiedname = "<Unclassified>" Then reclassifiedname = ""

                        text.WriteLine(i + 1 & ";" & _BD.LandUseBlock(i).ActualName & ";" & _BD.LandUseBlock(i).ReclassifiedName)
                    Next
            End Select
        End Using
    End Sub

    'Carrega o arquivo de texto com os nomes das classes
    'tipo = "Soil" ou "Landuse"
    Public Sub LoadClassNamesFromFile(ByVal Filename As String, ByVal tipo As String)

        If tipo <> "Soil" AndAlso tipo <> "Landuse" Then Throw New ArgumentException("Class type " & tipo & " is not known.")

        Using text As New IO.StreamReader(Filename)

            Dim linha() As String
            text.ReadLine() 'Cabeçalho

            Select Case tipo
                Case "Soil"
                    For i = 0 To _BD.SoilBlock.Count - 1
                        linha = text.ReadLine.Split(";")
                        _BD.SoilBlock(i).ActualName = linha(1)
                        _BD.SoilBlock(i).ReclassifiedName = linha(2)

                    Next
                Case "Landuse"
                    For i = 0 To _BD.LandUseBlock.Count - 1
                        linha = text.ReadLine.Split(";")
                        _BD.LandUseBlock(i).ActualName = linha(1)
                        _BD.LandUseBlock(i).ReclassifiedName = linha(2)
                    Next
            End Select
        End Using
    End Sub

    ''' <summary>
    ''' Soma os rasters de uso do solo e tipo do solo
    ''' </summary>
    Public Sub SumLandUse_SoilTyperasters()

        _HRUraster = Nothing
        GC.Collect() 'Limpa a memória

        If _LandUseVeget.Linhas <> _SoilTypes.Linhas Then Throw New Exception("LU and ST number of raster lines doesn't match.")
        If _LandUseVeget.Colunas <> _SoilTypes.Colunas Then Throw New Exception("LU and ST number of raster columns doesn't match.")
        If _LandUseVeget.Cellsize <> _SoilTypes.Cellsize Then Throw New Exception("LU and ST number of raster cellsize doesn't match")

        _HRUraster = _SoilTypes.CopyData 'Copia os dados para o novo raster

        For l = 0 To _HRUraster.Linhas - 1
            For c = 0 To _HRUraster.Colunas - 1
                If _HRUraster.Dados(l, c) = _HRUraster.NoDataValue OrElse _LandUseVeget.Dados(l, c) = _LandUseVeget.NoDataValue Then 'Se não for noData
                    _HRUraster.Dados(l, c) = _HRUraster.NoDataValue
                Else
                    _HRUraster.Dados(l, c) += (_LandUseVeget.Dados(l, c) * 100) 'Soma os rasters, com o LandUse * 100
                End If
            Next
        Next
    End Sub

End Class

''' <summary>
''' Classe que opera na forma de banco de dados de blocos
''' </summary>
Public Class BlockDatabase

    Public LandUseBlock As List(Of BlockClasses) 'Lista de classes de uso do solo e cobertura vegetal
    Public SoilBlock As List(Of BlockClasses) 'Lista de classes de tipo de solo
    Public HRUBlock As List(Of HRUClass) 'Lista de URHs

    ''' <summary>
    ''' Cria uma nova instância da classe
    ''' </summary>
    Public Sub New()
        LandUseBlock = New List(Of BlockClasses)
        SoilBlock = New List(Of BlockClasses)
        HRUBlock = New List(Of HRUClass)
    End Sub

    'Verifica se o nome composto das classes de uso de solo + tipo de solo existem
    Friend Function IdentifyComposedNameEnumerator(ByVal landuseBlock As BlockClasses, ByVal soiltypeBlock As BlockClasses) As Integer

        Dim tempHRU As New HRUClass(landuseBlock, soiltypeBlock)

        For z = 0 To HRUBlock.Count - 1
            If HRUBlock(z).ComposedName = tempHRU.ComposedName Then Return z
        Next

        Return -1 'Se não encontrar

    End Function

    'Identifica o total de nomes reclassificados para o uso do solo e cobertura vegetal
    Public Function GetLandUseReclassifiedNames() As String()
        Dim str(0) As String
        Dim hasName As Boolean = False
        Dim count As Integer = 0

        For i = 0 To LandUseBlock.Count - 1
            hasName = False
            For j = 0 To str.Count - 1
                If LandUseBlock.Item(i).ReclassifiedName = str(j) Then
                    hasName = True : Exit For
                End If
            Next

            If hasName = False Then
                ReDim Preserve str(count)
                str(count) = LandUseBlock.Item(i).ReclassifiedName
                count += 1
            End If
        Next
        Return str
    End Function

    'Identifica o total de nomes reclassificados para o tipo de solo
    Public Function GetSoilTypeReclassifiedNames() As String()
        Dim str(0) As String
        Dim hasName As Boolean = False
        Dim count As Integer = 0

        For i = 0 To SoilBlock.Count - 1
            hasName = False
            For j = 0 To str.Count - 1
                If SoilBlock.Item(i).ReclassifiedName = str(j) Then
                    hasName = True : Exit For
                End If
            Next

            If hasName = False Then
                ReDim Preserve str(count)
                str(count) = SoilBlock.Item(i).ReclassifiedName
                count += 1
            End If
        Next
        Return str
    End Function

    'Identifica todas as combinações possíveis para os nomes d
    Public Sub IdentifyHRUCombinations()

        HRUBlock.Clear()

        Dim count As Integer = 0
        Dim composedNameEnum As Integer

        For i = 0 To LandUseBlock.Count - 1
            For j = 0 To SoilBlock.Count - 1

                composedNameEnum = IdentifyComposedNameEnumerator(LandUseBlock(i), SoilBlock(j))

                If composedNameEnum = -1 Then 'Caso não houver
                    'Adiciona a composição
                    HRUBlock.Add(New HRUClass(LandUseBlock(i), SoilBlock(j)))
                    count += 1
                    'Adiciona a numeração para o bloco recém adicionado
                    HRUBlock(count - 1).AddRasterTemporaryID(LandUseBlock(i).RasterIdentifier * 100 + SoilBlock(j).RasterIdentifier, LandUseBlock(i).RasterIdentifier * 100)
                Else
                    'Adiciona a numeração correspondente ao bloco existente
                    HRUBlock(composedNameEnum).AddRasterTemporaryID(LandUseBlock(i).RasterIdentifier * 100 + SoilBlock(j).RasterIdentifier, LandUseBlock(i).RasterIdentifier * 100)
                End If
            Next
        Next
    End Sub

    'Identifica o número final do raster das URHs
    Public Sub SetFinalHRU_ID(ByVal Water_block_Name As String)

        Dim FinalNumber As Int16 = 1
        Dim count As Integer = 0
        Dim ComposedNames As New List(Of String)
        Dim FoundWaterBlock As Boolean = False

        'Adiciona os nomes compostos, sem repetição
        For i = 0 To HRUBlock.Count - 1

            If ExistComposedName(HRUBlock(i).ComposedName, ComposedNames) = False Then
                If Not HRUBlock(i).ComposedName = Water_block_Name Then
                    ComposedNames.Add(HRUBlock(i).ComposedName)
                Else
                    FoundWaterBlock = True
                End If
            End If
        Next

        If FoundWaterBlock = False Then Throw New Exception("Water HRU name does not match with any class.")
        ComposedNames.Add(Water_block_Name)

        For i = 0 To HRUBlock.Count - 1

            For j = 0 To ComposedNames.Count - 1
                If HRUBlock(i).ComposedName = ComposedNames.Item(j) Then
                    HRUBlock(i).SetFinalID(j + 1)
                    Exit For
                End If
            Next
        Next

    End Sub

    Private Function ExistComposedName(ByVal value As String, ByVal list As List(Of String)) As Boolean
        For k = 0 To list.Count - 1
            If list.Item(k) = value Then Return True
        Next
        Return False
    End Function

End Class

''' <summary>
''' Classe para criação dos blocos de uso do solo, cobertura vegetal e tipo do solo
''' </summary>
Public Class BlockClasses
    Private _rasterIDNumber As Integer
    Private _ActualName As String
    Private _ReclassifiedName As String
    Private _CountNumber As Long
    Private _percentOfCountNumber As Single

    'Número identificador (ID) do arquivo raster
    Public ReadOnly Property RasterIdentifier As Integer
        Get
            Return _rasterIDNumber
        End Get
    End Property
    'Nome da Classe
    Public Property ActualName As String
        Set(ByVal value As String)
            If value = "" Then
                _ActualName = "<Unknown Class>"
            Else
                _ActualName = value
            End If
        End Set
        Get
            Return _ActualName
        End Get
    End Property
    'Nome reclassificado para a classe
    Public Property ReclassifiedName As String
        Set(ByVal value As String)
            If value = "" Then
                _ReclassifiedName = "<Unclassified>"
            Else
                _ReclassifiedName = value
            End If
        End Set
        Get
            Return _ReclassifiedName
        End Get
    End Property
    'Contagem do número de células para a classe
    Public ReadOnly Property Count As Long
        Get
            Return _CountNumber
        End Get
    End Property
    'Percentual do número de células para a classe
    Public ReadOnly Property Percent As Single
        Get
            Return Math.Round((_percentOfCountNumber) * 100, 2)
        End Get
    End Property

    Public Sub New()
        _ActualName = "<Unknown Class>"
        _ReclassifiedName = "<Unclassified>"
    End Sub

    'Atribui o identificador da classe
    Friend Sub SetRasterNumber(ByVal ID As Integer)
        _rasterIDNumber = ID
    End Sub
    'Atribui o número de células contadas para a classe
    Friend Sub SetCountNumber(ByVal value As Long)
        _CountNumber = value
    End Sub
    'Atribui o percentual de células contadas para a classe
    Friend Sub SetPercentNumber(ByVal value As Single)
        _percentOfCountNumber = value
    End Sub
End Class

''' <summary>
''' Classe que armazena as URHs
''' </summary>
Public Class HRUClass
    Private _rasterComposedIDNumber As List(Of Integer) 'Land Use + SoilType
    Private _rasterSoilTypeDisabledIDNumber As List(Of Integer) 'Land Use somente
    Private _rasterFinalIDNumber As Int16
    Private _landUseVegetationBlock As BlockClasses
    Private _SoilTypeBlock As BlockClasses
    Private _Status As BlockStatus

    Public Sub New()
        _rasterComposedIDNumber = New List(Of Integer)
        _rasterSoilTypeDisabledIDNumber = New List(Of Integer)
    End Sub

    'Enumerador que indica o status do bloco, se for utilizado a combinação ou somente o uso do solo
    Public Enum BlockStatus

        Enabled = 0
        SoilTypeDisabled = 1

    End Enum

    'Identificador (ID) composto pela combinação do ID de tipo de solo e de uso do solo e cobertura vegetal
    Public ReadOnly Property ComposedID As String
        Get
            Dim line As String = ""

            For x = 0 To _rasterComposedIDNumber.Count - 1
                line += CStr(_rasterComposedIDNumber.Item(x))
                If x < (_rasterComposedIDNumber.Count - 1) Then line += ";"
            Next

            Return line
        End Get
    End Property

    'Identificador final da composição da URH
    Public ReadOnly Property FinalRasterID As Int16
        Get
            Return _rasterFinalIDNumber
        End Get
    End Property

    'Nome composto pela combinação da classe de uso do solo e cobertura vegetal + tipo de solo
    Public ReadOnly Property ComposedName As String
        Get
            If _Status = BlockStatus.Enabled Then
                Return _landUseVegetationBlock.ReclassifiedName & " + " & _SoilTypeBlock.ReclassifiedName
            ElseIf _Status = BlockStatus.SoilTypeDisabled Then
                Return _landUseVegetationBlock.ReclassifiedName
            Else
                Return "Blockstatus não atribuído!"
            End If
        End Get
    End Property

    'Retorna ou atribui o estado do bloco
    Public Property Status As BlockStatus
        Set(ByVal value As BlockStatus)
            _Status = value
        End Set
        Get
            Return _Status
        End Get
    End Property

    'Cria uma nova instância da classe a partir da combinação do uso de solo e cobertura vegetal + tipo de solo
    Public Sub New(ByVal LandUseBlock As BlockClasses, ByVal SoilBlock As BlockClasses)
        _landUseVegetationBlock = LandUseBlock
        _SoilTypeBlock = SoilBlock

        _rasterComposedIDNumber = New List(Of Integer)
        _rasterSoilTypeDisabledIDNumber = New List(Of Integer)
    End Sub

    'Atribui o identificador da classe composta
    Friend Sub AddRasterTemporaryID(ByVal valueComposed As Integer, ByVal valueSoilTypeDisabled As Integer)

        _rasterSoilTypeDisabledIDNumber.Add(valueSoilTypeDisabled)
        _rasterComposedIDNumber.Add(valueComposed)
    End Sub

    Public Sub SetFinalID(ByVal value As Int16)
        _rasterFinalIDNumber = value
    End Sub

End Class
