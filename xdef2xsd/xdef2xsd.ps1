Param(
[System.String]$input_dir,
[System.String]$output_dir,
[System.String]$tool_path
)
[System.String]$input_fx = "xdef"

function OutputXsd($file)
{
	Add-Type -Assembly System.IO
    $out_path = [System.IO.Path]::Combine($output_dir, $file.BaseName + ".xsd")
    $pinfo = New-Object System.Diagnostics.ProcessStartInfo
    $pinfo.FileName = [System.IO.Path]::Combine($tool_path, "xdef2xsd.exe")
    $pinfo.Arguments = @("-i"; $file.FullName; "-o"; $out_path)
	$pinfo.UseShellExecute = $false
    $p = New-Object System.Diagnostics.Process
    $p.StartInfo = $pinfo
    $p.Start() | Out-Null
    $p.WaitForExit()
}

Get-ChildItem ($input_dir + "\*." + $input_fx) | ForEach-Object { OutputXsd($_) }