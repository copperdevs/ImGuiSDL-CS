using System.Runtime.CompilerServices;
using System.Text;
using System.Runtime.InteropServices;
using System.Numerics;
using System.Diagnostics;
using ImGuiNET;
using static SDL3.SDL;

namespace ImGuiSDL;

public unsafe class ImGuiRenderer : IDisposable
{
	/// <summary>
	/// Custom ImGui User-Callback
	/// </summary>
	public delegate void UserCallback(ImDrawListPtr parentList, ImDrawCmdPtr cmd); 

	/// <summary>
	/// SDL GPU Device
	/// </summary>
	public readonly nint Device;

	/// <summary>
	/// SDL Window
	/// </summary>
	public readonly nint Window;

	/// <summary>
	/// ImGui Context
	/// </summary>
	public readonly nint Context;

	/// <summary>
	/// Scales all of the ImGui Content by this amount
	/// </summary>
	public float Scale = 1.0f;

	private readonly nint vertexShader;
	private readonly nint fragmentShader;
	private readonly GpuBuffer vertexBuffer;
	private readonly GpuBuffer indexBuffer;
	private readonly nint pipeline;
	private readonly nint fontTexture;
	private readonly nint sampler;

	private ImDrawVert[] vertices = [];
	private ushort[] indices = [];
	private readonly List<UserCallback> callbacks = [];
	private readonly Stopwatch timer = Stopwatch.StartNew();
	private TimeSpan time = TimeSpan.Zero;

	public ImGuiRenderer(nint sdlGpuDevice, nint sdlWindow, nint imGuiContext)
	{
		var io = ImGui.GetIO();

		Device = sdlGpuDevice;
		Window = sdlWindow;
		Context = imGuiContext;

		// get shader mode
		var driver = SDL_GetGPUDeviceDriver(Device);
		var shaderFormat = driver switch
		{
			"private" => SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_PRIVATE,
			"vulkan" => SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_SPIRV,
			"direct3d12" => SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_DXIL,
			"metal" => SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_MSL,
			_ => throw new NotImplementedException()
		};
		var shaderExt = shaderFormat switch
        {
            SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_PRIVATE => "spv",
            SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_SPIRV => "spv",
            SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_DXIL => "dxil",
            SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_MSL => "msl",
			_ => throw new NotImplementedException()
        };

		// create Vertex and Fragment shaders
		{
			var vertexCode = GetEmbeddedBytes($"ImGui.vertex.{shaderExt}");
			var vertexEntry = Encoding.UTF8.GetBytes("vertex_main");

			var fragmentCode = GetEmbeddedBytes($"ImGui.fragment.{shaderExt}");
			var fragmentEntry = Encoding.UTF8.GetBytes("fragment_main");

			fixed (byte* vertexCodePtr = vertexCode)
			fixed (byte* vertexEntryPtr = vertexEntry)
			{
				vertexShader = SDL_CreateGPUShader(Device, new()
				{
					code_size = (uint)vertexCode.Length,
					code = vertexCodePtr,
					entrypoint = vertexEntryPtr,
					format = shaderFormat,
					stage = SDL_GPUShaderStage.SDL_GPU_SHADERSTAGE_VERTEX,
					num_samplers = 0,
					num_storage_textures = 0,
					num_storage_buffers = 0,
					num_uniform_buffers = 1
				});

				if (vertexShader == nint.Zero)
					throw new Exception($"{nameof(SDL_CreateGPUShader)} Failed: {SDL_GetError()}");
			}

			fixed (byte* fragmentCodePtr = fragmentCode)
			fixed (byte* fragmentEntryPtr = fragmentEntry)
			{
				fragmentShader = SDL_CreateGPUShader(Device, new()
				{
					code_size = (uint)fragmentCode.Length,
					code = fragmentCodePtr,
					entrypoint = fragmentEntryPtr,
					format = shaderFormat,
					stage = SDL_GPUShaderStage.SDL_GPU_SHADERSTAGE_FRAGMENT,
					num_samplers = 1,
					num_storage_textures = 0,
					num_storage_buffers = 0,
					num_uniform_buffers = 0
				});

				if (fragmentShader == nint.Zero)
					throw new Exception($"{nameof(SDL_CreateGPUShader)} Failed: {SDL_GetError()}");
			}
		}

		// create graphics pipeline
		{
			var colorTargetDesc = stackalloc SDL_GPUColorTargetDescription[1] {
				new() {
					format = SDL_GetGPUSwapchainTextureFormat(Device, Window),
					blend_state = new()
					{
						src_color_blendfactor = SDL_GPUBlendFactor.SDL_GPU_BLENDFACTOR_SRC_ALPHA,
						dst_color_blendfactor = SDL_GPUBlendFactor.SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
						color_blend_op = SDL_GPUBlendOp.SDL_GPU_BLENDOP_ADD,
						src_alpha_blendfactor = SDL_GPUBlendFactor.SDL_GPU_BLENDFACTOR_ONE,
						dst_alpha_blendfactor = SDL_GPUBlendFactor.SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
						alpha_blend_op = SDL_GPUBlendOp.SDL_GPU_BLENDOP_ADD,
						enable_blend = true,
						enable_color_write_mask = false,
					}
				}
			};

			var vertexBuffDesc = stackalloc SDL_GPUVertexBufferDescription[1] {
				new() {
					slot = 0,
					pitch = sizeof(float) * 4 + sizeof(uint), // float2 + float2 + ubyte4
					input_rate = SDL_GPUVertexInputRate.SDL_GPU_VERTEXINPUTRATE_VERTEX,
					instance_step_rate = 0
				}
			};

			var vertexAttr = stackalloc SDL_GPUVertexAttribute[3] {
				// Position : float2
				new() { 
					format = SDL_GPUVertexElementFormat.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, 
					location = 0, 
					offset = 0
				},
				// TexCoord : float2
				new() { 
					format = SDL_GPUVertexElementFormat.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, 
					location = 1, 
					offset = sizeof(float) * 2
				},
				// Color: float4
				new() { 
					format = SDL_GPUVertexElementFormat.SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM, 
					location = 2, 
					offset = sizeof(float) * 4
				}
			};

			pipeline = SDL_CreateGPUGraphicsPipeline(Device, new()
			{
				vertex_shader = vertexShader,
				fragment_shader = fragmentShader,
				vertex_input_state = new()
				{
					vertex_buffer_descriptions = vertexBuffDesc,
					num_vertex_buffers = 1,
					vertex_attributes = vertexAttr,
					num_vertex_attributes = 3,
				},
				primitive_type = SDL_GPUPrimitiveType.SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
				rasterizer_state = new()
				{
					cull_mode = SDL_GPUCullMode.SDL_GPU_CULLMODE_NONE,
				}, 
				multisample_state = new(),
				depth_stencil_state = new(),
				target_info = new()
				{
					num_color_targets = 1,
					color_target_descriptions = colorTargetDesc
				}
			});
		}

		// create buffers
		{
			vertexBuffer = new(Device, SDL_GPUBufferUsageFlags.SDL_GPU_BUFFERUSAGE_VERTEX);
			indexBuffer = new(Device, SDL_GPUBufferUsageFlags.SDL_GPU_BUFFERUSAGE_INDEX);
		}

		// create sampler
		{
			sampler = SDL_CreateGPUSampler(Device, new() {
				min_filter = SDL_GPUFilter.SDL_GPU_FILTER_NEAREST,
				mag_filter = SDL_GPUFilter.SDL_GPU_FILTER_NEAREST,
				mipmap_mode = SDL_GPUSamplerMipmapMode.SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
				address_mode_u = SDL_GPUSamplerAddressMode.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
				address_mode_v = SDL_GPUSamplerAddressMode.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
				address_mode_w = SDL_GPUSamplerAddressMode.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
			});
		}

		// setup imgui font
		{
			io.Fonts.GetTexDataAsRGBA32(out byte* pixels, out int width, out int height, out int bytesPerPixel);
			var size = width * height * 4;

			// create texture
			fontTexture = SDL_CreateGPUTexture(Device, new()
			{
				type = SDL_GPUTextureType.SDL_GPU_TEXTURETYPE_2D,
				format = SDL_GPUTextureFormat.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
				usage = SDL_GPUTextureUsageFlags.SDL_GPU_TEXTUREUSAGE_SAMPLER,
				width = (uint)width,
				height = (uint)height,
				layer_count_or_depth = 1,
				num_levels = 1,
				sample_count = SDL_GPUSampleCount.SDL_GPU_SAMPLECOUNT_1
			});
			if (fontTexture == nint.Zero)
				throw new Exception($"{nameof(SDL_CreateGPUTexture)} Failed: {SDL_GetError()}");

			// upload texture data
			var transferBuffer = SDL_CreateGPUTransferBuffer(Device, new() {
				usage = SDL_GPUTransferBufferUsage.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
				size = (uint)size
			});

			var transferPtr = SDL_MapGPUTransferBuffer(Device, transferBuffer, false);
			Buffer.MemoryCopy(pixels, (void*)transferPtr, size, size);
			SDL_UnmapGPUTransferBuffer(Device, transferBuffer);

			var cmd = SDL_AcquireGPUCommandBuffer(Device);
			var pass = SDL_BeginGPUCopyPass(cmd);

			SDL_UploadToGPUTexture(pass,
				source: new() {
					transfer_buffer = transferBuffer,
					offset = 0,
				},
				destination: new() {
					texture = fontTexture,
					w = (uint)width,
					h = (uint)height,
					d = 1
				},
				cycle: false
			);
			SDL_EndGPUCopyPass(pass);
			SDL_SubmitGPUCommandBuffer(cmd);
			SDL_ReleaseGPUTransferBuffer(Device, transferBuffer);
		}

		// set imgui font texture id
		io.Fonts.SetTexID(fontTexture);
	}

	~ImGuiRenderer()
		=> Dispose();

	/// <summary>
	/// Begins a new Frame (calls <see cref="ImGui.NewFrame"/> internally)
	/// </summary>
	public void NewFrame()
	{
		SDL_GetWindowSizeInPixels(Window, out int windowWidth, out int windowHeight);

		ImGui.NewFrame();
		callbacks.Clear();

		var io = ImGui.GetIO();
		io.DeltaTime = (float)(timer.Elapsed - time).TotalSeconds;
		io.DisplaySize = new Vector2(windowWidth, windowHeight) / Scale;
		io.DisplayFramebufferScale = Vector2.One * Scale;
		time = timer.Elapsed;

		// toggle keyboard input
		if (io.WantTextInput && !SDL_TextInputActive(Window))
			SDL_StartTextInput(Window);
		else if (!io.WantTextInput && SDL_TextInputActive(Window))
			SDL_StopTextInput(Window);
	}

	/// <summary>
	/// Ends the ImGui Frame (calls <see cref="ImGui.EndFrame"/> internally)
	/// </summary>
	public void EndFrame()
	{
		ImGui.EndFrame();
	}

	/// <summary>
	/// Adds a UserCallback method
	/// </summary>
	public void AddUserCallback(UserCallback callback, nint? userData = null)
	{
		callbacks.Add(callback);
		ImGui.GetWindowDrawList().AddCallback(callbacks.Count, userData ?? nint.Zero);
	}

	/// <summary>
	/// Allow ImGui to process an SDL Event
	/// </summary>
	public void ProcessEvent(SDL_Event ev)
	{
		var io = ImGui.GetIO();

		switch ((SDL_EventType)ev.type)
		{
			// mouse input:

			case SDL_EventType.SDL_EVENT_MOUSE_MOTION:
				io.MousePos = new Vector2(ev.motion.x, ev.motion.y) / Scale;
				break;

			case SDL_EventType.SDL_EVENT_MOUSE_BUTTON_DOWN:
			case SDL_EventType.SDL_EVENT_MOUSE_BUTTON_UP:
				io.AddMouseButtonEvent(ev.button.button - 1, ev.button.down);
				break;

			case SDL_EventType.SDL_EVENT_MOUSE_WHEEL:
				io.AddMouseWheelEvent(ev.wheel.x, ev.wheel.y);
				break;

			// keyboard input:
			case SDL_EventType.SDL_EVENT_KEY_DOWN:
			case SDL_EventType.SDL_EVENT_KEY_UP:
				io.AddKeyEvent(ImGuiKey.ModCtrl, (ev.key.mod & SDL_Keymod.SDL_KMOD_CTRL) != 0);
				io.AddKeyEvent(ImGuiKey.ModShift, (ev.key.mod & SDL_Keymod.SDL_KMOD_SHIFT) != 0);
				io.AddKeyEvent(ImGuiKey.ModAlt, (ev.key.mod & SDL_Keymod.SDL_KMOD_ALT) != 0);
				io.AddKeyEvent(ImGuiKey.ModSuper, (ev.key.mod & SDL_Keymod.SDL_KMOD_GUI) != 0);
				io.AddKeyEvent(GetImGuiKey((SDL_Keycode)ev.key.key, ev.key.scancode), ev.key.down);
				break;

			case SDL_EventType.SDL_EVENT_TEXT_INPUT:
				ImGuiNative.ImGuiIO_AddInputCharactersUTF8(io.NativePtr, ev.text.text);
				break;
		}
	}

	/// <summary>
	/// Destroys the contents of the Renderer
	/// </summary>
	public void Dispose()
	{
		GC.SuppressFinalize(this);

		SDL_ReleaseGPUShader(Device, vertexShader);
		SDL_ReleaseGPUShader(Device, fragmentShader);
		SDL_ReleaseGPUGraphicsPipeline(Device, pipeline);
		SDL_ReleaseGPUTexture(Device, fontTexture);
		SDL_ReleaseGPUSampler(Device, sampler);

		vertexBuffer.Dispose();
		indexBuffer.Dispose();
	}

	/// <summary>
	/// Renders the ImGuiContents (calls <see cref="SDL_WaitAndAcquireGPUSwapchainTexture"/> and <see cref="ImGui.Render"/> internally)
	/// </summary>
	public void Render(SDL_FColor? clearColor = null)
	{
		var cmd = SDL_AcquireGPUCommandBuffer(Device);

		if (SDL_WaitAndAcquireGPUSwapchainTexture(cmd, Window, out var swapchain, out var width, out var height))
		{
			Render(cmd, swapchain, (int)width, (int)height, clearColor);
		}
		else
		{
			Console.WriteLine($"{nameof(SDL_WaitAndAcquireGPUSwapchainTexture)} failed: {SDL_GetError()}");
		}

		SDL_SubmitGPUCommandBuffer(cmd);
	}

	/// <summary>
	/// Renders the ImGuiContents (calls <see cref="ImGui.Render"/> internally)
	/// </summary>
	public void Render(nint cmd, nint swapchainTexture, int swapchainWidth, int swapchainHeight, SDL_FColor? clearColor = null)
	{
		var lastContext = ImGui.GetCurrentContext();
		ImGui.SetCurrentContext(Context);

		var io = ImGui.GetIO();
		io.DisplaySize = new Vector2(swapchainWidth, swapchainHeight) / Scale;

		// render data
		ImGui.Render();

		// vaidate data
		var data = ImGui.GetDrawData();
		if (data.NativePtr == null || data.TotalVtxCount <= 0)
		{
			ImGui.SetCurrentContext(lastContext);
			return;
		}

		// build vertex/index buffer lists
		{
			// calculate total size
			var vertexCount = 0;
			var indexCount = 0;
			for (int i = 0; i < data.CmdListsCount; i ++)
			{
				vertexCount += data.CmdLists[i].VtxBuffer.Size;
				indexCount += data.CmdLists[i].IdxBuffer.Size;
			}

			// make sure we have enough space
			if (vertexCount > vertices.Length)
				Array.Resize(ref vertices, vertexCount);
			if (indexCount > indices.Length)
				Array.Resize(ref indices, indexCount);

			// copy data to arrays
			vertexCount = indexCount = 0;
			for (int i = 0; i < data.CmdListsCount; i ++)
			{
				var list = data.CmdLists[i];
				var vertexSrc = new Span<ImDrawVert>((void*)list.VtxBuffer.Data, list.VtxBuffer.Size);
				var indexSrc = new Span<ushort>((void*)list.IdxBuffer.Data, list.IdxBuffer.Size);

				vertexSrc.CopyTo(vertices.AsSpan()[vertexCount..]);
				indexSrc.CopyTo(indices.AsSpan()[indexCount..]);

				vertexCount += vertexSrc.Length;
				indexCount += indexSrc.Length;
			}

			// begin GPU copy pass (upload buffers)
			var copy = SDL_BeginGPUCopyPass(cmd);
			vertexBuffer.Upload<ImDrawVert>(copy, vertices.AsSpan(0, vertexCount));
			indexBuffer.Upload<ushort>(copy, indices.AsSpan(0, indexCount));
			SDL_EndGPUCopyPass(copy);
		}

		// render contents
		{
			SDL_GPUColorTargetInfo colorTargetInfo = new()
			{
				texture = swapchainTexture,
				clear_color = clearColor ?? default,
				load_op = clearColor.HasValue ? SDL_GPULoadOp.SDL_GPU_LOADOP_CLEAR : SDL_GPULoadOp.SDL_GPU_LOADOP_LOAD,
				store_op = SDL_GPUStoreOp.SDL_GPU_STOREOP_STORE,
			};

			scoped ref var depthTarget = ref Unsafe.NullRef<SDL_GPUDepthStencilTargetInfo>();
			var pass = SDL_BeginGPURenderPass(cmd, [ colorTargetInfo ], 1, depthTarget);

			// bind pipeline
			SDL_BindGPUGraphicsPipeline(pass, pipeline);

			// bind fragment samplers
			var texture = fontTexture;
			SDL_BindGPUFragmentSamplers(pass, 0, [new() { sampler = sampler, texture = fontTexture }], 1);

			// bind buffers
			SDL_BindGPUIndexBuffer(pass, new() { buffer = indexBuffer.Buffer }, SDL_GPUIndexElementSize.SDL_GPU_INDEXELEMENTSIZE_16BIT);
			SDL_BindGPUVertexBuffers(pass, 0, [new() { buffer = vertexBuffer.Buffer }], 1);

			// set matrix uniform
			{
				Matrix4x4 mat =
					Matrix4x4.CreateScale(data.FramebufferScale.X, data.FramebufferScale.Y, 1.0f) *
					Matrix4x4.CreateOrthographicOffCenter(0, swapchainWidth, swapchainHeight, 0, 0, 100.0f);
				SDL_PushGPUVertexUniformData(cmd, 0, new nint(&mat), (uint)Marshal.SizeOf<Matrix4x4>());
			}

			// draw commands
			var globalVtxOffset = 0;
			var globalIdxOffset = 0;
			for (int i = 0; i < data.CmdListsCount; i ++)
			{
				var imList = data.CmdLists[i];
				var imCommands = (ImDrawCmd*)imList.CmdBuffer.Data;

				for (var imCmd = imCommands; imCmd < imCommands + imList.CmdBuffer.Size; imCmd++)
				{
					if (imCmd->UserCallback != nint.Zero)
					{
						var index = (int)imCmd->UserCallback - 1;
						if (index >= 0 && index < callbacks.Count)
							callbacks[(int)imCmd->UserCallback - 1].Invoke(imList, imCmd);
						continue;
					}

					if (imCmd->TextureId != texture)
					{
						texture = imCmd->TextureId;
						SDL_BindGPUFragmentSamplers(pass, 0, [new() { sampler = sampler, texture = fontTexture }], 1);
					}

					SDL_SetGPUScissor(pass, new()
					{
						x = (int)(imCmd->ClipRect.X * data.FramebufferScale.X),
						y = (int)(imCmd->ClipRect.Y * data.FramebufferScale.Y),
						w = (int)((imCmd->ClipRect.Z - imCmd->ClipRect.X) * data.FramebufferScale.X),
						h = (int)((imCmd->ClipRect.W - imCmd->ClipRect.Y) * data.FramebufferScale.Y),
					});

					SDL_DrawGPUIndexedPrimitives(
						render_pass: pass,
						num_indices: imCmd->ElemCount,
						num_instances: 1,
						first_index: (uint)(imCmd->IdxOffset + globalIdxOffset),
						vertex_offset: (int)(imCmd->VtxOffset + globalVtxOffset),
						first_instance: 0
					);
				}

				globalVtxOffset += imList.VtxBuffer.Size;
				globalIdxOffset += imList.IdxBuffer.Size;
			}

			SDL_EndGPURenderPass(pass);
		}

		// return ImGui context
		ImGui.SetCurrentContext(lastContext);
	}

	private class GpuBuffer(nint device, SDL_GPUBufferUsageFlags usage) : IDisposable
	{
		public readonly nint Device = device;
		public readonly SDL_GPUBufferUsageFlags Usage = usage;
		public nint Buffer { get; private set; }
		public int Capacity { get; private set; }

		~GpuBuffer() => Dispose();

        public void Dispose()
        {
			GC.SuppressFinalize(this);

			if (Buffer != nint.Zero)
			{
				SDL_ReleaseGPUBuffer(Device, Buffer);
				Buffer = nint.Zero;
			}
        }

        public void Upload<T>(nint copyPass, in ReadOnlySpan<T> data) where T : unmanaged
		{
			var dataSize = Marshal.SizeOf<T>() * data.Length;

			// Create a new buffer if our size is too large
			if (Buffer == nint.Zero || dataSize > Capacity)
			{
				if (Buffer != nint.Zero)
				{
					SDL_ReleaseGPUBuffer(Device, Buffer);
					Buffer = nint.Zero;
				}

				Capacity = Math.Max(Capacity, 8);
				while (Capacity < dataSize)
					Capacity *= 2;

				Buffer = SDL_CreateGPUBuffer(Device, new()
				{
					usage = Usage,
					size = (uint)Capacity
				});

				if (Buffer == nint.Zero)
					throw new Exception($"{nameof(SDL_CreateGPUBuffer)} Failed: {SDL_GetError()}");
			}

			// TODO: cache this! reuse transfer buffer!
			var transferBuffer = SDL_CreateGPUTransferBuffer(Device, new()
			{
				usage = SDL_GPUTransferBufferUsage.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
				size = (uint)dataSize
			});

			// copy data
			fixed (T* src = data)
			{
				byte* dst = (byte*)SDL_MapGPUTransferBuffer(Device, transferBuffer, false);
                System.Buffer.MemoryCopy(src, dst, dataSize, dataSize);
				SDL_UnmapGPUTransferBuffer(Device, transferBuffer);
			}

			// submit to the GPU
			SDL_UploadToGPUBuffer(copyPass,
				source: new()
				{
					transfer_buffer = transferBuffer,
					offset = 0
				},
				destination: new()
				{
					buffer = Buffer,
					offset = 0,
					size = (uint)dataSize
				},
				cycle: false
			);

			// release transfer buffer
			SDL_ReleaseGPUTransferBuffer(Device, transferBuffer);
		}
	}

	private static byte[] GetEmbeddedBytes(string file)
	{
		var assembly = typeof(ImGuiRenderer).Assembly;
		using var stream = assembly.GetManifestResourceStream(file);
		if (stream != null)
		{
			var result = new byte[stream.Length];
			stream.ReadExactly(result);
			return result;
		}

		throw new Exception($"Failed to load Embedded file '{file}'");
	}

	ImGuiKey GetImGuiKey(SDL_Keycode keycode, SDL_Scancode scancode)
	{
		// Keypad doesn't have individual key values in SDL3
		switch (scancode)
		{
			case SDL_Scancode.SDL_SCANCODE_KP_0: return ImGuiKey.Keypad0;
			case SDL_Scancode.SDL_SCANCODE_KP_1: return ImGuiKey.Keypad1;
			case SDL_Scancode.SDL_SCANCODE_KP_2: return ImGuiKey.Keypad2;
			case SDL_Scancode.SDL_SCANCODE_KP_3: return ImGuiKey.Keypad3;
			case SDL_Scancode.SDL_SCANCODE_KP_4: return ImGuiKey.Keypad4;
			case SDL_Scancode.SDL_SCANCODE_KP_5: return ImGuiKey.Keypad5;
			case SDL_Scancode.SDL_SCANCODE_KP_6: return ImGuiKey.Keypad6;
			case SDL_Scancode.SDL_SCANCODE_KP_7: return ImGuiKey.Keypad7;
			case SDL_Scancode.SDL_SCANCODE_KP_8: return ImGuiKey.Keypad8;
			case SDL_Scancode.SDL_SCANCODE_KP_9: return ImGuiKey.Keypad9;
			case SDL_Scancode.SDL_SCANCODE_KP_PERIOD: return ImGuiKey.KeypadDecimal;
			case SDL_Scancode.SDL_SCANCODE_KP_DIVIDE: return ImGuiKey.KeypadDivide;
			case SDL_Scancode.SDL_SCANCODE_KP_MULTIPLY: return ImGuiKey.KeypadMultiply;
			case SDL_Scancode.SDL_SCANCODE_KP_MINUS: return ImGuiKey.KeypadSubtract;
			case SDL_Scancode.SDL_SCANCODE_KP_PLUS: return ImGuiKey.KeypadAdd;
			case SDL_Scancode.SDL_SCANCODE_KP_ENTER: return ImGuiKey.KeypadEnter;
			case SDL_Scancode.SDL_SCANCODE_KP_EQUALS: return ImGuiKey.KeypadEqual;
			default: break;
		}
		switch (keycode)
		{
			case SDL_Keycode.SDLK_TAB: return ImGuiKey.Tab;
			case SDL_Keycode.SDLK_LEFT: return ImGuiKey.LeftArrow;
			case SDL_Keycode.SDLK_RIGHT: return ImGuiKey.RightArrow;
			case SDL_Keycode.SDLK_UP: return ImGuiKey.UpArrow;
			case SDL_Keycode.SDLK_DOWN: return ImGuiKey.DownArrow;
			case SDL_Keycode.SDLK_PAGEUP: return ImGuiKey.PageUp;
			case SDL_Keycode.SDLK_PAGEDOWN: return ImGuiKey.PageDown;
			case SDL_Keycode.SDLK_HOME: return ImGuiKey.Home;
			case SDL_Keycode.SDLK_END: return ImGuiKey.End;
			case SDL_Keycode.SDLK_INSERT: return ImGuiKey.Insert;
			case SDL_Keycode.SDLK_DELETE: return ImGuiKey.Delete;
			case SDL_Keycode.SDLK_BACKSPACE: return ImGuiKey.Backspace;
			case SDL_Keycode.SDLK_SPACE: return ImGuiKey.Space;
			case SDL_Keycode.SDLK_RETURN: return ImGuiKey.Enter;
			case SDL_Keycode.SDLK_ESCAPE: return ImGuiKey.Escape;
			case SDL_Keycode.SDLK_APOSTROPHE: return ImGuiKey.Apostrophe;
			case SDL_Keycode.SDLK_COMMA: return ImGuiKey.Comma;
			case SDL_Keycode.SDLK_MINUS: return ImGuiKey.Minus;
			case SDL_Keycode.SDLK_PERIOD: return ImGuiKey.Period;
			case SDL_Keycode.SDLK_SLASH: return ImGuiKey.Slash;
			case SDL_Keycode.SDLK_SEMICOLON: return ImGuiKey.Semicolon;
			case SDL_Keycode.SDLK_EQUALS: return ImGuiKey.Equal;
			case SDL_Keycode.SDLK_LEFTBRACKET: return ImGuiKey.LeftBracket;
			case SDL_Keycode.SDLK_BACKSLASH: return ImGuiKey.Backslash;
			case SDL_Keycode.SDLK_RIGHTBRACKET: return ImGuiKey.RightBracket;
			case SDL_Keycode.SDLK_GRAVE: return ImGuiKey.GraveAccent;
			case SDL_Keycode.SDLK_CAPSLOCK: return ImGuiKey.CapsLock;
			case SDL_Keycode.SDLK_SCROLLLOCK: return ImGuiKey.ScrollLock;
			case SDL_Keycode.SDLK_NUMLOCKCLEAR: return ImGuiKey.NumLock;
			case SDL_Keycode.SDLK_PRINTSCREEN: return ImGuiKey.PrintScreen;
			case SDL_Keycode.SDLK_PAUSE: return ImGuiKey.Pause;
			case SDL_Keycode.SDLK_LCTRL: return ImGuiKey.LeftCtrl;
			case SDL_Keycode.SDLK_LSHIFT: return ImGuiKey.LeftShift;
			case SDL_Keycode.SDLK_LALT: return ImGuiKey.LeftAlt;
			case SDL_Keycode.SDLK_LGUI: return ImGuiKey.LeftSuper;
			case SDL_Keycode.SDLK_RCTRL: return ImGuiKey.RightCtrl;
			case SDL_Keycode.SDLK_RSHIFT: return ImGuiKey.RightShift;
			case SDL_Keycode.SDLK_RALT: return ImGuiKey.RightAlt;
			case SDL_Keycode.SDLK_RGUI: return ImGuiKey.RightSuper;
			case SDL_Keycode.SDLK_APPLICATION: return ImGuiKey.Menu;
			case SDL_Keycode.SDLK_0: return ImGuiKey._0;
			case SDL_Keycode.SDLK_1: return ImGuiKey._1;
			case SDL_Keycode.SDLK_2: return ImGuiKey._2;
			case SDL_Keycode.SDLK_3: return ImGuiKey._3;
			case SDL_Keycode.SDLK_4: return ImGuiKey._4;
			case SDL_Keycode.SDLK_5: return ImGuiKey._5;
			case SDL_Keycode.SDLK_6: return ImGuiKey._6;
			case SDL_Keycode.SDLK_7: return ImGuiKey._7;
			case SDL_Keycode.SDLK_8: return ImGuiKey._8;
			case SDL_Keycode.SDLK_9: return ImGuiKey._9;
			case SDL_Keycode.SDLK_A: return ImGuiKey.A;
			case SDL_Keycode.SDLK_B: return ImGuiKey.B;
			case SDL_Keycode.SDLK_C: return ImGuiKey.C;
			case SDL_Keycode.SDLK_D: return ImGuiKey.D;
			case SDL_Keycode.SDLK_E: return ImGuiKey.E;
			case SDL_Keycode.SDLK_F: return ImGuiKey.F;
			case SDL_Keycode.SDLK_G: return ImGuiKey.G;
			case SDL_Keycode.SDLK_H: return ImGuiKey.H;
			case SDL_Keycode.SDLK_I: return ImGuiKey.I;
			case SDL_Keycode.SDLK_J: return ImGuiKey.J;
			case SDL_Keycode.SDLK_K: return ImGuiKey.K;
			case SDL_Keycode.SDLK_L: return ImGuiKey.L;
			case SDL_Keycode.SDLK_M: return ImGuiKey.M;
			case SDL_Keycode.SDLK_N: return ImGuiKey.N;
			case SDL_Keycode.SDLK_O: return ImGuiKey.O;
			case SDL_Keycode.SDLK_P: return ImGuiKey.P;
			case SDL_Keycode.SDLK_Q: return ImGuiKey.Q;
			case SDL_Keycode.SDLK_R: return ImGuiKey.R;
			case SDL_Keycode.SDLK_S: return ImGuiKey.S;
			case SDL_Keycode.SDLK_T: return ImGuiKey.T;
			case SDL_Keycode.SDLK_U: return ImGuiKey.U;
			case SDL_Keycode.SDLK_V: return ImGuiKey.V;
			case SDL_Keycode.SDLK_W: return ImGuiKey.W;
			case SDL_Keycode.SDLK_X: return ImGuiKey.X;
			case SDL_Keycode.SDLK_Y: return ImGuiKey.Y;
			case SDL_Keycode.SDLK_Z: return ImGuiKey.Z;
			case SDL_Keycode.SDLK_F1: return ImGuiKey.F1;
			case SDL_Keycode.SDLK_F2: return ImGuiKey.F2;
			case SDL_Keycode.SDLK_F3: return ImGuiKey.F3;
			case SDL_Keycode.SDLK_F4: return ImGuiKey.F4;
			case SDL_Keycode.SDLK_F5: return ImGuiKey.F5;
			case SDL_Keycode.SDLK_F6: return ImGuiKey.F6;
			case SDL_Keycode.SDLK_F7: return ImGuiKey.F7;
			case SDL_Keycode.SDLK_F8: return ImGuiKey.F8;
			case SDL_Keycode.SDLK_F9: return ImGuiKey.F9;
			case SDL_Keycode.SDLK_F10: return ImGuiKey.F10;
			case SDL_Keycode.SDLK_F11: return ImGuiKey.F11;
			case SDL_Keycode.SDLK_F12: return ImGuiKey.F12;
			case SDL_Keycode.SDLK_F13: return ImGuiKey.F13;
			case SDL_Keycode.SDLK_F14: return ImGuiKey.F14;
			case SDL_Keycode.SDLK_F15: return ImGuiKey.F15;
			case SDL_Keycode.SDLK_F16: return ImGuiKey.F16;
			case SDL_Keycode.SDLK_F17: return ImGuiKey.F17;
			case SDL_Keycode.SDLK_F18: return ImGuiKey.F18;
			case SDL_Keycode.SDLK_F19: return ImGuiKey.F19;
			case SDL_Keycode.SDLK_F20: return ImGuiKey.F20;
			case SDL_Keycode.SDLK_F21: return ImGuiKey.F21;
			case SDL_Keycode.SDLK_F22: return ImGuiKey.F22;
			case SDL_Keycode.SDLK_F23: return ImGuiKey.F23;
			case SDL_Keycode.SDLK_F24: return ImGuiKey.F24;
			case SDL_Keycode.SDLK_AC_BACK: return ImGuiKey.AppBack;
			case SDL_Keycode.SDLK_AC_FORWARD: return ImGuiKey.AppForward;
			default: break;
		}
		return ImGuiKey.None;
	}
}
