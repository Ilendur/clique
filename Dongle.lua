--[[-------------------------------------------------------------------------
  Copyright (c) 2006, Dongle Development Team
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above
        copyright notice, this list of conditions and the following
        disclaimer in the documentation and/or other materials provided
        with the distribution.
      * Neither the name of the Dongle Development Team nor the names of 
        its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
---------------------------------------------------------------------------]]

local major,minor = "DongleStub", 1
local g = getfenv(0)

if not g.DongleStub or g.DongleStub:IsNewerVersion(major, minor) then
	local lib = setmetatable({}, {
		__call = function(t,k) 
			if type(t.versions == "table") and t.versions[k] then 
				return t.versions[k] 
			else
				error("Cannot find a library with name '"..tostring(k).."'")
			end
		end
	})

	function lib:IsNewerVersion(major, minor)
		local entry = self.versions and self.versions[major]
		
		if not entry then return true end
		local oldmajor,oldminor = entry:GetVersion()
		
		return minor > oldminor
	end
	
	function lib:Register(new)
		local major,minor = new:GetVersion()
		if not self:IsNewerVersion(major, minor) then return false end
		local old = self.versions and self.versions[major]
		-- Run the new libraries activation
		new:Activate(old)
		
		-- Deactivate the old libary if necessary
		if old and old.Deactivate then old:Deactivate(new) end
		
		self.versions[major] = new
	end

	function lib:GetVersion() return major,minor end

	function lib:Activate(old)
		if old then 
			self.versions = old.versions
		else
			self.versions = {}
		end
		g.DongleStub = self
	end
	
	-- Actually trigger libary activation here
	local stub = g.DongleStub or lib
	stub:Register(lib)
end

--[[-------------------------------------------------------------------------
Begin Library Implementation
---------------------------------------------------------------------------]]

local major = "Dongle"
local minor = tonumber(select(3,string.find("$Revision: 52 $", "(%d+)")) or 1)

assert(DongleStub, string.format("%s requires DongleStub.", major))
if not DongleStub:IsNewerVersion(major, minor) then return end

Dongle = {}
local methods = {
	"RegisterEvent", "UnregisterEvent", "UnregisterAllEvents", "FireEvent",
	"EnableDebug", "Print", "Debug",
	"InitializeDB", "RegisterDBDefaults", "ResetDB",
	"SetProfile", "CopyProfile", "ResetProfile", "DeleteProfile",
	"NewModule", "HasModule", "IterateModules",
}

local registry = {}
local lookup = {}
local loadqueue = {}
local loadorder = {}
local events = {}

local function assert(level,condition,message)
	if not condition then
		error(message,level)
	end
end

local function argcheck(level, value, num, ...)
	assert(1, type(level) == "number",
		"Bad argument #1 to 'argcheck' (number expected, got " .. type(level) .. ")")
	assert(1, type(num) == "number",
		"Bad argument #3 to 'argcheck' (number expected, got " .. type(level) .. ")")

	for i=1,select("#", ...) do
		if type(value) == select(i, ...) then return end
	end

	local name = string.match(debugstack(level), "in function ['`<](.-)['`>]")
	local types = strjoin(", ", ...)

	error(string.format("Bad argument #%d to '%s' (%s expected, got %s)",
		num, name or "Unknown", types, type(value)), level)
end

function Dongle:New(obj, name)
	argcheck(3, obj, 2, "table", "string", "nil")
	argcheck(3, name, 3, "string", "nil")

	if not name and type(obj) == "string" then
		name = obj
		obj = {}
	end

	if registry[name] then
		error("A Dongle with the name '"..name.."' is already registered.")
	end

	local reg = {["obj"] = obj, ["name"] = name}

	registry[name] = reg
	lookup[obj] = reg
	lookup[name] = reg

	for k,v in pairs(methods) do
		obj[v] = self[v]
	end

	-- Add this Dongle to the end of the queue
	table.insert(loadqueue, obj)
	return obj,name
end

function Dongle:NewModule(obj, name)
	local reg = lookup[self]
	assert(2, reg, "You must call 'NewModule' from a registered Dongle.")

	obj,name = Dongle:New(obj, name)

	if not reg.modules then reg.modules = {} end
	table.insert(reg.modules, name)
	table.sort(reg.modules)
end

function Dongle:HasModule(name)
	local reg = lookup[self]
	assert(2, reg, "You must call 'HasModule' from a registered Dongle.")

	return lookup[name]
end

local EMPTY_TABLE = {}

function Dongle:IterateModules()
	local reg = lookup[self]
	assert(2, reg, "You must call 'IterateModules' from a registered Dongle.")

	return ipairs(reg.modules or EMPTY_TABLE)
end

function Dongle:ADDON_LOADED(frame, event, ...)
	for i=1, #loadqueue do
		local obj = loadqueue[i]
		table.insert(loadorder, obj)

		if type(obj.Initialize) == "function" then
			obj:Initialize()
		end

		if self.initialized and type(obj.Enable) == "function" then
			obj:Enable()
		end
		loadqueue[i] = nil
	end
end

function Dongle:PLAYER_LOGIN()
	self.initialized = true
	for i,obj in ipairs(loadorder) do
		if type(obj.Enable) == "function" then
			obj:Enable()
		end
	end
end

function Dongle:FireEvent(frame, event, ...)
	argcheck(3, event, 2, "string")

	local eventTbl = events[event]
	if eventTbl then
		for obj,func in pairs(eventTbl) do
			if type(func) == "string" then
				if type(obj[func]) then
					obj[func](obj, event, ...)
				end
			else
				func(event, ...)
			end
		end
	end
end

function Dongle:RegisterEvent(event, func)
	local reg = lookup[self]
	assert(2, reg, "You must call 'RegisterEvent' from a registered Dongle.")
	argcheck(3, event, 2, "string")
	argcheck(3, func, 3, "string", "function", "nil")

	-- Name the method the same as the event if necessary
	if not func then func = event end

	if not events[event] then
		events[event] = {}
		frame:RegisterEvent(event)
	end
	events[event][self] = func
end

function Dongle:UnregisterEvent(event)
	local reg = lookup[self]
	assert(2, reg, "You must call 'NewModule' from a registered Dongle.")
	argcheck(3, event, "string")

	if events[event] then
		events[event][self] = nil
		if not next(events[event]) then
			events[event] = nil
			frame:UnregisterEvent(event)
		end
	end
end

function Dongle:UnregisterAllEvents()
	assert(2, lookup[self], "You must call 'NewModule' from a registered Dongle.")

	for event,tbl in pairs(events) do
		for k,v in pairs(tbl) do
			k[self] = nil
		end
	end
end

function Dongle:AdminEvents(event)
	local method
	if event == "PLAYER_LOGOUT" then
		Dongle:ClearDBDefaults()
		method = "Disable"
	elseif event == "PLAYER_REGEN_DISABLED" then
		method = "CombatLockdown"
	elseif event == "PLAYER_REGEN_ENABLED" then
		method = "CombatUnlock"
	end

	if method then
		for k,v in pairs(registry) do
			local obj = v.obj
			if obj[method] then obj[method](obj) end
		end
	end
end

function Dongle:EnableDebug(level)
	local reg = lookup[self]
	assert(2, reg, "You must call 'EnableDebug' from a registered Dongle.")
	argcheck(3, level, "number", "nil")

	reg.debugLevel = level
end

do
	local function printHelp(obj, method, msg, ...)
		local reg = lookup[obj]
		assert(2, reg, "You must call '"..method.."' from a registered Dongle.")

		local name = reg.name
		local success,txt = pcall(string.format, "|cFF33FF99%s|r: %s", name, msg)
		if success then
			ChatFrame1:AddMessage(string.format(txt, ...))
		else
			error(string.gsub(txt, "'%?'", string.format("%s:%s()", name, "Print")), 3)
		end
	end

	function Dongle:Print(msg, ...)
		return printHelp(self, "Print", msg, ...)
	end

	function Dongle:Debug(level, msg, ...)
		local reg = lookup[self]
		if reg.debugLevel and level >= reg.debugLevel then
			printHelp(self, "Debug", msg, ...)
		end
	end
end

function Dongle:InitializeDB(name, defaults)
	local reg = lookup[self]
	assert(2, reg, "You must call 'InitializeDB' from a registered Dongle.")
	argcheck(3, name, 2, "string")
	argcheck(3, defaults, 3, "table", "nil")

	local sv = getglobal(name)

	if not sv then
		sv = {}
		setglobal(name, sv)

		-- Lets do the initial setup
		sv.char = {}
		sv.faction = {}
		sv.realm = {}
		sv.class = {}
		sv.global = {}
		sv.profiles = {}
	end

	-- Initialize the specific databases
	local char = string.format("%s of %s", UnitName("player"), GetRealmName())
	local realm = string.format("%s", GetRealmName())
	local class = UnitClass("player")
	local race = select(2, UnitRace("player"))
	local faction = UnitFactionGroup("player")

	-- Initialize the containers
	if not sv.char then sv.char = {} end
	if not sv.realm then sv.realm = {} end
	if not sv.class then sv.class = {} end
	if not sv.faction then sv.faction = {} end
	if not sv.global then sv.global = {} end
	if not sv.profiles then sv.profiles = {} end

	-- Initialize this characters profiles
	if not sv.char[char] then sv.char[char] = {} end
	if not sv.realm[realm] then sv.realm[realm] = {} end
	if not sv.class[class] then sv.class[class] = {} end
	if not sv.faction[faction] then sv.faction[faction] = {} end

	-- Try to get the profile selected from the char db
	local profileKey = sv.char[char].profileKey or char
	sv.char[char].profileKey = profileKey

	if not sv.profiles[profileKey] then sv.profiles[profileKey] = {} end

	local db = {
		["char"] = sv.char[char],
		["realm"] = sv.realm[realm],
		["class"] = sv.class[class],
		["faction"] = sv.faction[faction],
		["profile"] = sv.profiles[profileKey],
		["global"] = sv.global,
		["profiles"] = sv.profiles,
	}

	local reg = lookup[self]
	reg.sv = sv
	reg.sv_name = name
	reg.db = db
	reg.db_char = char
	reg.db_realm = realm
	reg.db_class = class
	reg.db_faction = faction
	reg.db_profileKey = profileKey

	if defaults then
		self:RegisterDBDefaults(db, defaults)
	end

	return db
end

local function copyDefaults(dest, src)
	for k,v in pairs(src) do
		if type(v) == "table" then
			if not dest[k] then dest[k] = {} end
			copyDefaults(dest[k], v)
		else
			dest[k] = v
		end
	end
end

function Dongle:RegisterDBDefaults(db, defaults)
	local reg = lookup[self]
	assert(2, reg, "You must call 'RegisterDBDefaults' from a registered Dongle.")
	argcheck(3, db, 2, "table")
	argcheck(3, defaults, 3, "table")
	assert(reg.db, "You cannot call \"RegisterDBDefaults\" before calling \"InitializeDB\".")

	if defaults.char then copyDefaults(db.char, defaults.char) end
	if defaults.realm then copyDefaults(db.realm, defaults.realm) end
	if defaults.class then copyDefaults(db.class, defaults.class) end
	if defaults.faction then copyDefaults(db.faction, defaults.faction) end
	if defaults.global then copyDefaults(db.global, defaults.global) end
	if defaults.profile then copyDefaults(db.profile, defaults.profile) end

	reg.dbDefaults = defaults
end

local function removeDefaults(db, defaults)
	for k,v in pairs(defaults) do
		if type(v) == "table" and db[k] then
			removeDefaults(db[k], v)
			if not next(v) then
				db[k] = nil
			end
		else
			if db[k] == defaults[k] then
				db[k] = nil
			end
		end
	end
end

function Dongle:ClearDBDefaults()
	for name,obj in pairs(registry) do
		local db = obj.db
		local defaults = obj.dbDefaults

		if db and defaults then
			if defaults.char then removeDefaults(db.char, defaults.char) end
			if defaults.realm then removeDefaults(db.realm, defaults.realm) end
			if defaults.class then removeDefaults(db.class, defaults.class) end
			if defaults.faction then removeDefaults(db.faction, defaults.faction) end
			if defaults.global then removeDefaults(db.global, defaults.global) end
			if defaults.profile then removeDefaults(db.profile, defaults.profile) end
		end

		-- Remove any blank "profiles"
		local sv = obj.sv

		if not next(db.char) then sv.char[obj.db_char] = nil end
		if not next(db.realm) then sv.realm[obj.db_realm] = nil end
		if not next(db.class) then sv.class[obj.db_class] = nil end
		if not next(db.faction) then sv.faction[obj.db_faction] = nil end
		if not next(db.global) then sv.global = nil end
		if not next(db.profile) then sv.profiles[obj.db_profileKey] = nil end
	end
end

function Dongle:SetProfile(name)
	local reg = lookup[self]
	assert(2, reg, "You must call 'SetProfile' from a registered Dongle.")
	argcheck(3, name, 2, "string")

	local old = reg.sv.profiles[reg.db_profileKey]

	local new = reg.sv.profiles[name]
	if not new then
		reg.sv.profiles[name] = {}
		new = reg.sv.profiles[name]
	end

	if reg.dbDefaults and reg.dbDefaults.profile then
		-- Remove the defaults from the old profile
		removeDefaults(old, reg.dbDefaults.profile)

		-- Inject the defaults into the new profile
		copyDefaults(new, reg.dbDefaults.profile)
	end

	reg.db.profile = new

	-- Save this new profile name in db.char
	reg.db.char.profileKey = name

	if type(self.ProfileChanged) == "function" then
		self:ProfileChanged(name)
		self:Print("Profile has been changed to '" .. name .. "'")
	end
end

function Dongle:DeleteProfile(name)
	local reg = lookup[self]
	assert(2, reg, "You must call 'DeleteProfile' from a registered Dongle.")
	argcheck(3, name, 2, "string")

	if self.db.char.profileKey == name then
		error("You cannot delete your active profile.  Change profiles, then attempt to delete.", 2)
	end

	reg.sv.profiles[name] = nil
	self:Print("Profile '"..name.."' has been deleted.")
end

function Dongle:CopyProfile(name)
	local reg = lookup[self]
	assert(2, reg, "You must call 'CopyProfile' from a registered Dongle.")
	argcheck(3, name, 2, "string")

	assert(reg.db.char.profileKey ~= name, "Source/Destination profile cannot be the same profile")
	assert(type(reg.sv.profiles[name]) == "table", "Profile \""..name.."\" doesn't exist.")

	local profile = reg.db.profile
	local source = reg.sv.profiles[name]

	-- Don't do a destructive copy, just do what we're told
	copyDefaults(profile, source)
	self:Print("Settings from '"..name.."' have been copied to your current profile.")
end

function Dongle:ResetProfile()
	local reg = lookup[self]
	assert(2, reg, "You must call 'ResetProfile' from a registered Dongle.")

	local profile = reg.db.profile

	for k,v in pairs(profile) do
		profile[k] = nil
	end
	if reg.dbDefaults and reg.dbDefaults.profile then
		copyDefaults(profile, reg.dbDefaults.profile)
	end
	self:Print("Profile '"..name.."' has been reset.")
end

function Dongle:ResetDB()
	local reg = lookup[self]
	assert(2, reg, "You must call 'ResetDB' from a registered Dongle.")

	setglobal(reg.sv_name, {})
	self:InitializeDB(reg.sv_name, reg.dbDefaults)
	self:Print("Database has been reset.")
end

-- Set up a basic slash command for /dongle and /reload
SLASH_RELOAD1 = "/reload"
SLASH_RELOAD2 = "/rl"
SlashCmdList["RELOAD"] = ReloadUI

SLASH_DONGLE1 = "/dongle"

SlashCmdList["DONGLE"] = function(msg)
	local s,e,cmd,args = string.find(msg, "([^%s]+)%s*(.*)")
	if not cmd then return end

	cmd = string.lower(cmd)

	if cmd == "enable" then
		local name,title,notes,enabled = GetAddOnInfo(args)
		if enabled then
			Dongle:Print("'%s' is already enabled.", args)
		elseif not name then
			Dongle:Print("'%s' is not a valid addon.", args)
		elseif args then
			EnableAddOn(args)
			Dongle:Print("Enabled AddOn '%s'", args)
		end
	elseif cmd == "disable" then
		local name,title,notes,enabled = GetAddOnInfo(args)
		if not name then
			Dongle:Print("'%s' is not a valid addon.", args)
		elseif not enabled then
			Dongle:Print("'%s' is already disabled.", args)
		elseif args then
			DisableAddOn(args)
			Dongle:Print("Disabled AddOn '%s'", args)
		end
	end
end

--]]

--[[-------------------------------------------------------------------------
  Begin DongleStub required functions and registration
---------------------------------------------------------------------------]]

function Dongle:GetVersion() return major,minor end

function Dongle:Activate(old)
	if old then
		self.registry = old.registry
		self.lookup = old.lookup
		self.loadqueue = old.loadqueue
		self.loadorder = old.loadorder
		self.events = old.events

		registry = self.registry
		lookup = self.lookup
		loadqueue = self.loadqueue
		loadorder = self.loadorder
		events = self.events

		frame = old.frame
		self.registry[major].obj = self
	else
		self.registry = registry
		self.lookup = lookup
		self.loadqueue = loadqueue
		self.loadorder = loadorder
		self.events = events

		local reg = {obj = self}
		registry[major] = reg
		lookup[self] = reg
		lookup[major] = reg
	end

	if not frame then
		frame = CreateFrame("Frame")
	end

	self.frame = frame
	frame:SetScript("OnEvent", function(...) self:FireEvent(...) end)

	-- Register for events using Dongle itself
	self:RegisterEvent("ADDON_LOADED")
	self:RegisterEvent("PLAYER_LOGIN")
	self:RegisterEvent("PLAYER_LOGOUT", "AdminEvents")
	self:RegisterEvent("PLAYER_REGEN_ENABLED", "AdminEvents")
	self:RegisterEvent("PLAYER_REGEN_DISABLED", "AdminEvents")

	-- Convert all the modules handles
	for name,obj in pairs(registry) do
		for k,v in ipairs(methods) do
			obj[k] = self[v]
		end
	end
end

function Dongle:Deactivate(new)
	lookup[self] = nil
	self:UnregisterAllEvents()
end

DongleStub:Register(Dongle)
