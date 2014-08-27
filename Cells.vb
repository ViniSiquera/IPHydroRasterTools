'*******************************************************************
'CLASSES ESPECÍFICAS DE CÉLULAS UTILIZADAS PELO PROGRAMA
'Criado por Vinícius Alencar Siqueira - 20/01/2014
'*******************************************************************
''' <summary>
''' Classe base da célula, para indicar a posição na matriz
''' </summary>
Public Class Cells
    Public x, y As Int16

    Public Sub New(ByVal linha As Int16, ByVal coluna As Int16)
        x = coluna
        y = linha
    End Sub

End Class

''' <summary>
''' Celula com indicação do valor heuristico para algoritmo de remoção de depressões
''' </summary>
Public Class HeuristicCell
    Inherits Cells

    Public CostFunction As Single
    Public RelParentX, RelParentY As Int16 'posições relativas de X e Y, de onde a célula vem

    Public Sub New(ByVal linha As Int16, ByVal coluna As Int16)
        MyBase.New(linha, coluna)
    End Sub

End Class

''' <summary>
''' Classe que armazena as células com os exutórios das bacias
''' </summary>
''' <remarks></remarks>
Public Class CellWatershed
    Inherits Cells

    Public Atributo As Int16
    Public Lat, lon As Double

    Public Sub New(ByVal latitude As Double, ByVal longitude As Double, ByVal nRows As Integer, ByVal nCols As Integer, ByVal cellsize As Double, ByVal xllCorner As Double, ByVal yllCorner As Double, ByVal atrib As Int16)

        MyBase.New(0, 0) 'Cria uma célula qualquer
        ConvertCoordToRowCol(latitude, longitude, nRows, nCols, cellsize, xllCorner, yllCorner) 'Converte a coordenada para linha e coluna
        Lat = latitude
        lon = longitude
        Atributo = atrib 'Atributo
    End Sub
    ''' <summary>
    ''' Converte a coordenada geográfica para linha e coluna
    ''' </summary>
    Public Sub ConvertCoordToRowCol(ByVal latitude As Double, ByVal longitude As Double, ByVal nRows As Integer, ByVal nCols As Integer, ByVal cellsize As Double, ByVal xllCorner As Double, ByVal yllCorner As Double)

        'Transformação das coordenadas do ponto em linhas e colunas
        'Lembrando que a posição (1,1) equivale a (0,0)
        Dim xllCenter, yllCenter As Double
        xllCenter = xllCorner + (cellsize / 2) 'Calcula a coordenada X do centro da célula no canto esquerdo
        yllCenter = yllCorner + (cellsize / 2) 'Calcula a coordenada Y do centro da célula no canto esquerdo

        x = CInt(Math.Round(((longitude - xllCenter) / cellsize), 0))
        y = CInt(Math.Round((nRows - 1) - (latitude - yllCenter) / cellsize, 0))

        If x < 0 OrElse x > nCols Then Throw New ArgumentException("A longitude " & longitude & "º está fora dos limites do MDE.")
        If y < 0 OrElse y > nRows Then Throw New ArgumentException("A latitude " & latitude & "º está fora dos limites do MDE.")

    End Sub

End Class
