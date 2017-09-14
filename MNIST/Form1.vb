Imports System.ComponentModel

Public Class Form1

    ' This BackgroundWorker is used to demonstrate the   
    ' preferred way of performing asynchronous operations.  
    Private WithEvents backgroundWorker1 As BackgroundWorker
    Private WithEvents backgroundWorker2 As BackgroundWorker

    Dim s_mnist As New Source_mnist()

    Dim c_space As Combinatorial_space

    Dim m As Mnist_img

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        m = New Mnist_img()

        c_space = New Combinatorial_space(m, Combinatorial_space.Learning_mode.unsupervised)

        Label1.Text = ""
        Label2.Text = ""
        Label3.Text = ""
        Label9.Text = ""

        Button6.Enabled = False
        Button7.Enabled = False

        Button8.Enabled = False
        Button9.Enabled = False

        backgroundWorker1 = New System.ComponentModel.BackgroundWorker()
        backgroundWorker1.WorkerReportsProgress = True
        backgroundWorker1.WorkerSupportsCancellation = True

        backgroundWorker2 = New System.ComponentModel.BackgroundWorker()
        backgroundWorker2.WorkerReportsProgress = True
        backgroundWorker2.WorkerSupportsCancellation = True

    End Sub

    Structure DrawTraning
        Dim image1 As Bitmap
        Dim image2 As Bitmap
        Dim image3 As Bitmap
        Dim image4 As Bitmap
        Dim label1 As String
        Dim label2 As String
        Dim label3 As String
    End Structure


    Public Function Mnist_step(show As Boolean) As DrawTraning

        s_mnist.Get_next()

        m.pic = s_mnist.pic
        m.label = s_mnist.label

        m.Sobel()

        m.Make_det_out()

        Dim DrawTraning As New DrawTraning With {
            .image1 = m.Draw_pic,
            .image2 = m.Draw_gr1,
            .image3 = m.Draw_out,
            .label1 = s_mnist.label
        }

        Return DrawTraning

    End Function

    'Private Sub Draw_pic(m As Mnist_img, ByRef pb As PictureBox)

    '   pb.Image = m.Draw_pic
    '   pb.Update()

    'End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        ProgressBar1.Value = 0
        Dim Draw As DrawTraning = Mnist_step(True)
        ProgressBar1.Value = 100
        PictureBox1.Image = Draw.image1
        PictureBox2.Image = Draw.image2
        PictureBox3.Image = Draw.image3
        Label1.Text = "Correct character: " & Draw.label1
    End Sub

    ' шаг обучения
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Button2.Enabled = False
        Button8.Enabled = True

        Button1.Enabled = False
        Button3.Enabled = False
        Button4.Enabled = False
        Button5.Enabled = False

        ProgressBar1.Value = 0

        Dim Draw As DrawTraning = Learn_step()
        Label2.Text = Draw.label2
        Label1.Text = "Correct character: " & Draw.label1
        Label3.Text = "Sample: " & Draw.label3 & " of 500"
        ' Label4.Text = "Progress: " & Draw & "%"

        ProgressBar1.Value = 100

        PictureBox1.Image = Draw.image1
        PictureBox2.Image = Draw.image2
        PictureBox3.Image = Draw.image3
        PictureBox4.Image = Draw.image4
        Button2.Enabled = True
        Button8.Enabled = False

        Button1.Enabled = True
        Button3.Enabled = True
        Button4.Enabled = True
        Button5.Enabled = True
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        If BackgroundWorkerTraningEpoch.IsBusy <> True Then
            Button1.Enabled = False
            Button2.Enabled = False
            Button4.Enabled = False
            Button3.Enabled = False

            Button5.Enabled = False
            Button9.Enabled = True

            ' Start the asynchronous operation.
            BackgroundWorkerTraningEpoch.RunWorkerAsync()
        End If

    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click

        If BackgroundWorkerTraningEpoch.WorkerSupportsCancellation = True Then
            ' Cancel the asynchronous operation.
            BackgroundWorkerTraningEpoch.CancelAsync()
        End If

    End Sub

    Private Sub BackgroundWorkerTraningEpoch_DoWork(sender As Object, e As DoWorkEventArgs) Handles BackgroundWorkerTraningEpoch.DoWork
        Dim worker As BackgroundWorker = CType(sender, BackgroundWorker)
        Dim MyObj As New Report()
        'Some random work. Your code would go here.
        For i = 1 To 500
            If (worker.CancellationPending = True) Then
                e.Cancel = True
                Exit For
            Else
                ' Perform a time consuming operation and report progress.
                Dim Result As DrawTraning = Learn_step()

                Result.label3 = i

                worker.ReportProgress(i / 500 * 100, Result)
            End If

        Next
    End Sub

    Private Sub BackgroundWotkerTraningEpoch_ProgressChanged(sender As Object, e As ProgressChangedEventArgs) Handles BackgroundWorkerTraningEpoch.ProgressChanged
        ProgressBar1.Value = e.ProgressPercentage
        Label2.Text = e.UserState.label2
        Label1.Text = "Correct character: " & e.UserState.label1
        Label3.Text = "Sample: " & e.UserState.label3 & " of 500"
        Label4.Text = "Progress: " & e.ProgressPercentage & "%"

        PictureBox1.Image = e.UserState.image1
        PictureBox2.Image = e.UserState.image2
        PictureBox3.Image = e.UserState.image3
        PictureBox4.Image = e.UserState.image4
    End Sub

    Private Sub BackgroundWotkerTraningEpoch_Completed(sender As Object, e As RunWorkerCompletedEventArgs) Handles BackgroundWorkerTraningEpoch.RunWorkerCompleted
        If e.Cancelled = True Then
            Label2.Text = "Canceled!"
        ElseIf e.Error IsNot Nothing Then
            Label2.Text = "Error: " & e.Error.Message
        Else
            Label2.Text = "Done!"
            Label4.Text = "Progress: 100%"
        End If
        Button5.Enabled = True
        Button9.Enabled = False
        Button1.Enabled = True
        Button2.Enabled = True
        Button3.Enabled = True
        Button4.Enabled = True
    End Sub


    Public Function Learn_step() As DrawTraning

        Dim Draw As DrawTraning = Mnist_step(True)

        c_space.Activate_clasters(m.out_bin)
        c_space.Add_new_clusters(m.out_bin)

        c_space.Modify_clasters_F()

        c_space.Consolidate_memory_unsl()

        c_space.internal_time += 1

        Draw.label2 = c_space.info()

        Draw.image4 = c_space.Draw_mem()

        Return Draw

    End Function

    Structure Dig_item
        Dim bin As BitArray
        Dim label As Integer
    End Structure


    Structure Report
        Dim image1 As Bitmap
        Dim label1 As String
        Dim label2 As String
        Dim label9 As String
    End Structure

    ' тест 
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        If backgroundWorker1.IsBusy <> True Then
            Button3.Enabled = False
            Button6.Enabled = True

            Button1.Enabled = False
            Button4.Enabled = False
            Button2.Enabled = False
            Button5.Enabled = False

            ' Start the asynchronous operation.
            backgroundWorker1.RunWorkerAsync()
        End If

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        If backgroundWorker1.WorkerSupportsCancellation = True Then
            ' Cancel the asynchronous operation.
            backgroundWorker1.CancelAsync()
        End If

    End Sub

    ' This event handler sets the Text property of the TextBox  
    ' control. It is called on the thread that created the   
    ' TextBox control, so the call is thread-safe.  
    '  
    ' BackgroundWorker is the preferred way to perform asynchronous  
    ' operations.  
    Private Sub BackgroundWorker1_RunWorkerCompleted(
    ByVal sender As Object,
    ByVal e As RunWorkerCompletedEventArgs) _
    Handles backgroundWorker1.RunWorkerCompleted
        If e.Cancelled = True Then
            Label2.Text = "Canceled!"
        ElseIf e.Error IsNot Nothing Then
            Label2.Text = "Error: " & e.Error.Message
        Else
            Label2.Text = "Done!"
            Label4.Text = "Progress: 100%"
        End If
        Button3.Enabled = True
        Button6.Enabled = False

        Button1.Enabled = True
        Button4.Enabled = True
        Button2.Enabled = True
        Button5.Enabled = True

    End Sub

    ' This event handler updates the progress.
    Private Sub BackgroundWorker1_ProgressChanged(ByVal sender As System.Object,
    ByVal e As ProgressChangedEventArgs) Handles backgroundWorker1.ProgressChanged
        ProgressBar1.Value = e.ProgressPercentage
        PictureBox1.Image = e.UserState.image1
        Label1.Text = "Recognized character: " & e.UserState.label1
        Label2.Text = e.UserState.label2
        Label9.Text = "Sample: " & e.UserState.label9 & " of 30000"
        Label4.Text = "Progress: " & e.ProgressPercentage & "%"
    End Sub

    ' This event handler is where the time-consuming work is done.
    Private Sub DoTest1(ByVal sender As System.Object,
    ByVal e As DoWorkEventArgs) Handles backgroundWorker1.DoWork
        Dim worker As BackgroundWorker = CType(sender, BackgroundWorker)

        'Some random work. Your code would go here.
        Dim items As New List(Of Dig_item)

        Dim s, s1, s2 As Integer
        Dim c As Double

        Dim max As Double
        Dim max_N As Integer
        Dim min_c As Double = 0.85

        Dim p_label As Integer
        Dim N_yes, N_no As Integer

        Dim answers(s_mnist.N_examples) As Boolean

        Dim MyObj As New Report()

        For i = 1 To s_mnist.N_examples
            If (worker.CancellationPending = True) Then
                e.Cancel = True
                Exit For
            Else
                ' Perform a time consuming operation and report progress.
                Mnist_step(False)

                max = 0
                max_N = 0

                If items.Count > 0 Then

                    For j = 0 To items.Count - 1

                        Mult(m.out_bin, items(j).bin, c)

                        If c > max Then
                            max = c
                            max_N = j
                        End If

                    Next

                    p_label = items(max_N).label

                    If p_label = m.label Then
                        N_yes += 1
                        answers(i) = True
                    Else
                        N_no += 1
                        MyObj.image1 = m.Draw_pic
                        MyObj.label1 = p_label
                    End If

                    s = 0
                    If i > 100 Then
                        For j = i - 99 To i
                            If answers(j) Then s += 1
                        Next
                    End If

                    s1 = 0
                    If i > 1000 Then
                        For j = i - 999 To i
                            If answers(j) Then s1 += 1
                        Next
                    End If

                    MyObj.label2 = i & " y=" & N_yes & " n=" & N_no & " items.Count" & items.Count & " last 100 yes=" & s & " last 1000 yes=" & s1

                End If

                If max < min_c Then

                    items.Add(New Dig_item With {.bin = m.out_bin.Clone, .label = m.label})

                End If

                MyObj.label9 = i
                worker.ReportProgress(i / s_mnist.N_examples * 100, MyObj)

            End If

        Next

    End Sub

    ' тест 
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        If backgroundWorker2.IsBusy <> True Then
            Button4.Enabled = False
            Button7.Enabled = True

            Button1.Enabled = False
            Button3.Enabled = False
            Button2.Enabled = False
            Button5.Enabled = False
            ' Start the asynchronous operation.
            backgroundWorker2.RunWorkerAsync()
        End If

    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        If backgroundWorker2.WorkerSupportsCancellation = True Then
            ' Cancel the asynchronous operation.
            backgroundWorker2.CancelAsync()
        End If

    End Sub

    ' This event handler sets the Text property of the TextBox  
    ' control. It is called on the thread that created the   
    ' TextBox control, so the call is thread-safe.  
    '  
    ' BackgroundWorker is the preferred way to perform asynchronous  
    ' operations.  
    Private Sub BackgroundWorker2_RunWorkerCompleted(
    ByVal sender As Object,
    ByVal e As RunWorkerCompletedEventArgs) _
    Handles backgroundWorker2.RunWorkerCompleted
        If e.Cancelled = True Then
            Label2.Text = "Canceled!"
        ElseIf e.Error IsNot Nothing Then
            Label2.Text = "Error: " & e.Error.Message
        Else
            Label2.Text = "Done!"
            Label4.Text = "Progress: 100%"
        End If
        Button4.Enabled = True
        Button7.Enabled = False

        Button1.Enabled = True
        Button3.Enabled = True
        Button2.Enabled = True
        Button5.Enabled = True
    End Sub

    ' This event handler updates the progress.
    Private Sub BackgroundWorker2_ProgressChanged(ByVal sender As System.Object,
    ByVal e As ProgressChangedEventArgs) Handles backgroundWorker2.ProgressChanged
        ProgressBar1.Value = e.ProgressPercentage
        PictureBox1.Image = e.UserState.image1
        Label1.Text = "Recognized character: " & e.UserState.label1
        Label2.Text = e.UserState.label2
        Label9.Text = "Sample: " & e.UserState.label9 & " of 30000"
        Label4.Text = "Progress: " & e.ProgressPercentage & "%"
    End Sub

    ' тест с бустингом
    Private Sub DoTest2(ByVal sender As System.Object,
    ByVal e As DoWorkEventArgs) Handles backgroundWorker2.DoWork
        Dim worker As BackgroundWorker = CType(sender, BackgroundWorker)

        'Some random work. Your code would go here.

        Dim items As New List(Of Dig_item)

        Dim s, s1, s2 As Integer
        Dim c As Double

        Dim max, max1 As Double
        Dim max_N As Integer
        Dim min_c As Double = 0.75

        Dim p_label As Integer
        Dim N_yes, N_no As Integer

        Dim N_Top As Integer = 10
        Dim vote(9) As Double
        Dim m_res As New List(Of Double)
        Dim m_lbl As New List(Of Integer)

        Dim m_res_array() As Double
        Dim m_lbl_array() As Integer

        Dim answers(s_mnist.N_examples) As Boolean

        Dim MyObj As New Report()

        For i = 1 To s_mnist.N_examples

            If (worker.CancellationPending = True) Then
                e.Cancel = True
                Exit For
            Else
                ' Perform a time consuming operation and report progress.
                Mnist_step(False)

                max = 0
                max_N = 0

                If items.Count > 0 Then

                    m_res.Clear()
                    m_lbl.Clear()

                    For j = 0 To items.Count - 1

                        Mult(m.out_bin, items(j).bin, c)

                        If c > max Then
                            max = c
                            max_N = j
                        End If

                        If c > 0.7 Then

                            m_res.Add(c)
                            m_lbl.Add(items(j).label)

                        End If

                    Next

                    m_res_array = m_res.ToArray
                    m_lbl_array = m_lbl.ToArray

                    Array.Sort(m_res_array, m_lbl_array)

                    If m_lbl_array.Length >= 1 Then

                        Array.Clear(vote, 0, 10)

                        For j = 1 To Math.Min(N_Top, m_lbl_array.Length)

                            vote(m_lbl_array(m_res_array.Length - j)) += m_res_array(m_res_array.Length - j)

                        Next

                        max1 = 0

                        max_N = -1

                        For j = 0 To 9
                            If vote(j) > max1 Then
                                max1 = vote(j)
                                max_N = j
                            End If
                        Next


                        p_label = max_N

                        If p_label = m.label Then
                            N_yes += 1
                            answers(i) = True
                        Else
                            N_no += 1
                            MyObj.image1 = m.Draw_pic
                            MyObj.label1 = p_label
                        End If

                    Else
                        N_no += 1
                    End If

                    s = 0
                    If i > 100 Then
                        For j = i - 99 To i
                            If answers(j) Then s += 1
                        Next
                    End If


                    If i Mod 10 = 0 Then

                        s1 = 0
                        If i > 1000 Then
                            For j = i - 999 To i
                                If answers(j) Then s1 += 1
                            Next
                        End If

                    End If

                    MyObj.label2 = i & " y=" & N_yes & " n=" & N_no & " items.Count" & items.Count & " last 100 yes=" & s & " last 1000 yes=" & s1

                End If

                If max < min_c Then

                    items.Add(New Dig_item With {.bin = m.out_bin.Clone, .label = m.label})

                End If

                MyObj.label9 = i
                worker.ReportProgress(i / s_mnist.N_examples * 100, MyObj)

            End If

        Next

    End Sub

    Private Sub Mult(ByRef bin1 As BitArray, ByRef bin2 As BitArray, ByRef c As Double)

        Dim s, s1, s2 As Integer

        For i = 0 To bin1.Length - 1

            s1 -= bin1(i)
            s2 -= bin2(i)

            s += bin1(i) * bin2(i)

        Next

        If s1 > 0 And s2 > 0 Then
            c = s / Math.Sqrt(s1 * s2)
        Else
            c = 0
        End If

    End Sub

End Class
