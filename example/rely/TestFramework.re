let projectDir =
  Pervasives.__FILE__
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname;

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir:
        Filename.(
          projectDir
          |> (dir => concat(dir, "example"))
          |> (dir => concat(dir, "rely"))
          |> (dir => concat(dir, "__snapshots__"))
        ),
      projectDir,
    });
});
