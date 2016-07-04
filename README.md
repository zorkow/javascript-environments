# javascript-environments
Environments for working with JavaScript and some of its libraries.

## How to install a js2 environment:

### Emacs packages:

     js2-mode
     closure-glslint

### Linters:

#### gjslint

Either installed via apt-get or npm.

## How to install a nodejs environment:

Install the js2 environment.

### Emacs packages:
   
     js-comint

### Node

Either installed with nvm or apt-get


## How to install a typescript environment:

### Node packages:

     sudo npm -g install typescript
     sudo npm -g install typescript-tools


   Possibly also:

     sudo npm -g install typescript-formatter



### Emacs packages:

     flycheck-typescript-tslint
     tide
     tss
     typescript
     typescript-mode

### Emacs integration: 
   
     (setq load-path (cons YOUR_LOCAL_PATH_TO_ELISP_DIR load-path))
     (load-library "js2-local")
     (load-library "typescript-local")

### Directory Structure:

    project_root/
    project_root/tsconfig.json
     /scripts
       /ts
       /js
     /tests
       /ts
       /js
       /doc

### Making a tsconfig.json file:

https://basarat.gitbooks.io/typescript/content/docs/project/tsconfig.html

http://techiejs.com/Blog/Post/Leveraging-tsconfigjson-in-TypeScript-projects

https://github.com/Microsoft/TypeScript/wiki/tsconfig.json

Very basic for the above structure:

     {
      "compilerOptions": {
         "target": "es5",
         "module": "commonjs",
         "declaration": false,
         "noImplicitAny": false,
         "removeComments": true,
         "noLib": false,
         "outDir": "scripts/js"
       }
    }

### Linting

#### tslint with standard unix formatter

    sudo npm -g install tslint
    npm install tslint-unix-formatter

Calls to the linter should then be made with:

    tslint -t unix -s ~/node_modules/tslint-unix-formatter/lib/

#### Setup and edit a tslint.json file for configuration.
   

#### gjslint

One can also use gjslint as this offers the fixjsstyle option. 
    
    sudo npm -g install gjslint
    apt-get install closure-linter


### Conventions

#### General Conventions:
https://github.com/Platypi/style_typescript

#### Naming conventions: 
https://gist.github.com/aleksey-bykov/34599f736d745fee7136

#### Commenting conventions:

JS Doc style
http://usejsdoc.org/
