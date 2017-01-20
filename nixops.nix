let
  region = "us-east-1";
  accessKeyId = "rmmdev";
  ec2 =
    { resources, ... }:
    {
      deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.ebsBoot = true;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.large";
      deployment.ec2.keyPair = resources.ec2KeyPairs.testKeyPair;
      deployment.ec2.subnetId = "subnet-c9b508be";
      deployment.ec2.associatePublicIpAddress = true;
    };
in
  {
    jenkins = args@{ resources, config, pkgs, ... }:
      ec2 args // {
        networking.firewall.allowedTCPPorts = [ 8080 ];
        services.jenkins = {
          enable = true;
          packages = [ pkgs.stdenv pkgs.git pkgs.jdk config.programs.ssh.package pkgs.nix pkgs.stack ];
        };
      };

    resources.ec2KeyPairs.testKeyPair = { inherit region accessKeyId; };
  }

