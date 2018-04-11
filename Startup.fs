namespace Starikov

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.EntityFrameworkCore
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders
open System


type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration
    member val Configuration : IConfiguration = null with get, set

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddTransient<IBaseSQLCommands, CafedraDBContext>() |> ignore
        //let token = Path.Combine(AppContext.BaseDirectory, "appsettings.json") |> JsonModule.GetBody |> fun body -> new JsonModule.JSON(body)
        services.AddSingleton<IBaseSQLCommands>(new CafedraDBContext(this.Configuration.["Data:Default:ConnectionString"])) |> ignore
        services.AddSingleton<IMessager>(new Messager((this.Configuration.["Messaging:Default:Email"]), (this.Configuration.["Messaging:Default:Password"]))) |> ignore
        services.AddSingleton<IAccountRegistrationHelper>(new AccountRegistrationHelper((this.Configuration.["RegistrationKeyWords:Default:ForAdmins"]), (this.Configuration.["RegistrationKeyWords:Default:ForCurators"]))) |> ignore
        services.AddSingleton<IFileProvider>(new PhysicalFileProvider(Path.Combine(Directory.GetCurrentDirectory(), "wwwroot"))) |> ignore
        services.AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme).AddCookie(fun options ->
            options.LoginPath <- new Microsoft.AspNetCore.Http.PathString("/Account/Login")
        ) |> ignore
        //services.AddDbContext<AppIdentityDbContext>(fun options -> options.UseSqlServer(@"Server=(localdb)\\MSSQLLocalDB;Database=Identity;Trusted_Connection=True;MultipleActiveResultSets=true", null)) |> ignore
        services.AddMvc() |> ignore     //this.Configuration.["Data:Default:ConnectionString"]
        services.AddAuthorization(fun option -> option.AddPolicy("CuratorOnly", (fun policy -> policy.RequireClaim("PSTU_Role", "curator") |> ignore))) |> ignore
        services.AddAuthorization(fun option -> option.AddPolicy("StudentOnly", (fun policy -> policy.RequireClaim("PSTU_Role", "student") |> ignore))) |> ignore
        services.AddMemoryCache() |> ignore
        services.AddSession() |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        //let bd = sprintf "%s" AppContext.BaseDirectory
        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore
            app.UseBrowserLink() |> ignore
        else
            app.UseExceptionHandler("/Home/Error") |> ignore
        app.UseStatusCodePages() |> ignore
        app.UseStaticFiles() |> ignore

        app.UseSession() |> ignore
        //app.UseCookieAuthentication()
        app.UseAuthentication() |> ignore
        app.UseMvc(fun routes ->
            routes.MapRoute(
                name = "default",
                template = "{controller=Home}/{action=Index}/{id?}") |> ignore
            ) |> ignore
        //app.Run(fun context -> context.Response.WriteAsync("Hello") )
        
        ()
    
