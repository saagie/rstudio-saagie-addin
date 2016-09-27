node {
    stage "checkout"
    checkout scm
    stage "build"
    env.WORKSPACE = pwd()
    sh 'docker pull saagie/r_builder'
    sh 'docker run --rm -v ' + env.WORKSPACE + ':/project saagie/r_builder'
    stage "publish artifact"
    archive '*.tar.gz'
}