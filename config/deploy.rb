set :keep_releases,       5
set :application,         "ts"
set :user,                "deploy"
set :deploy_to,           "/var/web/#{application}"
set :repository,          "git@github.com:spawnfest/weedit.git"
set :branch,              "master"
set :scm,                 :git
set :use_sudo,            false
set :deploy_via,          :remote_cache
set :term,                "linux"

ssh_options[:forward_agent] = true
ssh_options[:port] = 2222

role :app, "typesocial.net"

namespace :deploy do

  # remove rails-focused deploy tasks
  [:finalize_update].each do |default_task|
    task default_task do 
      # ... ahh, silence!
    end
  end

  namespace :ts do

    task :build, :roles => :app do
      run "cd /var/web/ts/current; make"
    end
  end

end

after "deploy:symlink", "deploy:ts:build"

