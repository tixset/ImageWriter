// SHA-256 implementation for Delphi 7
// Based on FIPS 180-2 specification
// Public domain implementation

unit sha256;

interface

uses
  Windows, SysUtils;

type
  TSHA256Digest = array[0..31] of Byte;
  TSHA256State = array[0..7] of Cardinal;
  TSHA256Block = array[0..15] of Cardinal;
  TSHA256Buffer = array[0..63] of Byte;
  
  TSHA256Context = record
    State: TSHA256State;
    Count: Int64;
    Buffer: TSHA256Buffer;
  end;

procedure SHA256Init(var Context: TSHA256Context);
procedure SHA256Update(var Context: TSHA256Context; Input: PChar; Length: Cardinal);
procedure SHA256Final(var Context: TSHA256Context; var Digest: TSHA256Digest);

function SHA256String(const S: string): TSHA256Digest;
function SHA256File(const FileName: string): TSHA256Digest;
function SHA256Print(const D: TSHA256Digest): string;
function SHA256Match(const D1, D2: TSHA256Digest): Boolean;

implementation

const
  // SHA-256 constants (first 32 bits of fractional parts of cube roots of first 64 primes)
  K: array[0..63] of Cardinal = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
  );

function ROR(x: Cardinal; n: Integer): Cardinal;
begin
  Result := (x shr n) or (x shl (32 - n));
end;

function Ch(x, y, z: Cardinal): Cardinal;
begin
  Result := (x and y) xor ((not x) and z);
end;

function Maj(x, y, z: Cardinal): Cardinal;
begin
  Result := (x and y) xor (x and z) xor (y and z);
end;

function Sigma0(x: Cardinal): Cardinal;
begin
  Result := ROR(x, 2) xor ROR(x, 13) xor ROR(x, 22);
end;

function Sigma1(x: Cardinal): Cardinal;
begin
  Result := ROR(x, 6) xor ROR(x, 11) xor ROR(x, 25);
end;

function Gamma0(x: Cardinal): Cardinal;
begin
  Result := ROR(x, 7) xor ROR(x, 18) xor (x shr 3);
end;

function Gamma1(x: Cardinal): Cardinal;
begin
  Result := ROR(x, 17) xor ROR(x, 19) xor (x shr 10);
end;

procedure SHA256Transform(var State: TSHA256State; const Block: TSHA256Block);
var
  a, b, c, d, e, f, g, h, T1, T2: Cardinal;
  W: array[0..63] of Cardinal;
  t: Integer;
begin
  // Prepare message schedule
  for t := 0 to 15 do
    W[t] := Block[t];
    
  for t := 16 to 63 do
    W[t] := Gamma1(W[t - 2]) + W[t - 7] + Gamma0(W[t - 15]) + W[t - 16];

  // Initialize working variables
  a := State[0];
  b := State[1];
  c := State[2];
  d := State[3];
  e := State[4];
  f := State[5];
  g := State[6];
  h := State[7];

  // Main loop
  for t := 0 to 63 do
  begin
    T1 := h + Sigma1(e) + Ch(e, f, g) + K[t] + W[t];
    T2 := Sigma0(a) + Maj(a, b, c);
    h := g;
    g := f;
    f := e;
    e := d + T1;
    d := c;
    c := b;
    b := a;
    a := T1 + T2;
  end;

  // Update state
  Inc(State[0], a);
  Inc(State[1], b);
  Inc(State[2], c);
  Inc(State[3], d);
  Inc(State[4], e);
  Inc(State[5], f);
  Inc(State[6], g);
  Inc(State[7], h);
end;

procedure SHA256Init(var Context: TSHA256Context);
begin
  FillChar(Context, SizeOf(Context), 0);
  
  // Initial hash values (first 32 bits of fractional parts of square roots of first 8 primes)
  Context.State[0] := $6a09e667;
  Context.State[1] := $bb67ae85;
  Context.State[2] := $3c6ef372;
  Context.State[3] := $a54ff53a;
  Context.State[4] := $510e527f;
  Context.State[5] := $9b05688c;
  Context.State[6] := $1f83d9ab;
  Context.State[7] := $5be0cd19;
  
  Context.Count := 0;
end;

procedure SHA256Update(var Context: TSHA256Context; Input: PChar; Length: Cardinal);
var
  Index, PartLen: Cardinal;
  Block: TSHA256Block;
  i: Integer;
begin
  Index := (Context.Count shr 3) and $3F;
  Inc(Context.Count, Length shl 3);
  PartLen := 64 - Index;

  if Length >= PartLen then
  begin
    Move(Input^, Context.Buffer[Index], PartLen);
    
    // Convert buffer to block (big-endian)
    for i := 0 to 15 do
      Block[i] := (Cardinal(Context.Buffer[i * 4]) shl 24) or
                  (Cardinal(Context.Buffer[i * 4 + 1]) shl 16) or
                  (Cardinal(Context.Buffer[i * 4 + 2]) shl 8) or
                  Cardinal(Context.Buffer[i * 4 + 3]);
    
    SHA256Transform(Context.State, Block);
    Inc(Input, PartLen);
    Dec(Length, PartLen);
    Index := 0;

    while Length >= 64 do
    begin
      // Convert input to block (big-endian)
      for i := 0 to 15 do
        Block[i] := (Cardinal(Input[i * 4]) shl 24) or
                    (Cardinal(Input[i * 4 + 1]) shl 16) or
                    (Cardinal(Input[i * 4 + 2]) shl 8) or
                    Cardinal(Input[i * 4 + 3]);
      
      SHA256Transform(Context.State, Block);
      Inc(Input, 64);
      Dec(Length, 64);
    end;
  end;

  if Length > 0 then
    Move(Input^, Context.Buffer[Index], Length);
end;

procedure SHA256Final(var Context: TSHA256Context; var Digest: TSHA256Digest);
var
  Bits: array[0..7] of Byte;
  Index, PadLen: Cardinal;
  Padding: array[0..63] of Byte;
  i: Integer;
begin
  // Save bit count (big-endian)
  for i := 0 to 7 do
    Bits[i] := (Context.Count shr ((7 - i) * 8)) and $FF;

  // Pad to 56 mod 64
  Index := (Context.Count shr 3) and $3F;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;

  FillChar(Padding, SizeOf(Padding), 0);
  Padding[0] := $80;
  SHA256Update(Context, @Padding, PadLen);

  // Append length
  SHA256Update(Context, @Bits, 8);

  // Store state in digest (big-endian)
  for i := 0 to 7 do
  begin
    Digest[i * 4] := (Context.State[i] shr 24) and $FF;
    Digest[i * 4 + 1] := (Context.State[i] shr 16) and $FF;
    Digest[i * 4 + 2] := (Context.State[i] shr 8) and $FF;
    Digest[i * 4 + 3] := Context.State[i] and $FF;
  end;

  FillChar(Context, SizeOf(Context), 0);
end;

function SHA256String(const S: string): TSHA256Digest;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PChar(S), Length(S));
  SHA256Final(Context, Result);
end;

function SHA256File(const FileName: string): TSHA256Digest;
var
  Context: TSHA256Context;
  F: THandle;
  Buffer: array[0..8191] of Char;
  BytesRead: DWORD;
begin
  SHA256Init(Context);
  
  F := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
                  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if F = INVALID_HANDLE_VALUE then
    raise Exception.Create('Cannot open file: ' + FileName);
    
  try
    while ReadFile(F, Buffer, SizeOf(Buffer), BytesRead, nil) and (BytesRead > 0) do
      SHA256Update(Context, @Buffer, BytesRead);
  finally
    CloseHandle(F);
  end;
  
  SHA256Final(Context, Result);
end;

function SHA256Print(const D: TSHA256Digest): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to 31 do
    Result := Result + IntToHex(D[i], 2);
  Result := LowerCase(Result);
end;

function SHA256Match(const D1, D2: TSHA256Digest): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to 31 do
    if D1[i] <> D2[i] then
    begin
      Result := False;
      Exit;
    end;
end;

end.
