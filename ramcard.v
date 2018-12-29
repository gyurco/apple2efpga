// taken from the Apple II project by Alex Freed
// and modified for own use

module ramcard(
  input mclk28,
  input reset_in,
  input PAGE2, HIRES, RAMRD, RAMWRT, STORE80, ALTZP,
  input [15:0] addr,
  output [15:0] ram_addr,
  output aux,
  input we,
  output card_ram_we,
  output card_ram_rd,
  output reg bank1
);

  reg read_en, write_en, pre_wr_en;
  reg [15:0] addr2;
  wire Dxxx;
  wire [7:0] ram_page = addr[15:8];
  wire aux_ram;

  always @(posedge mclk28) begin
    addr2 <= addr;
    if(reset_in) begin
      bank1 <= 0;
      read_en <= 0;
      write_en <= 1;
      pre_wr_en <= 0;
    end 
    else 
    begin
      if((addr[15:4] == 'hC08) & (addr2 != addr)) begin
        // Looks like a Language Card in slot 0
        bank1 <= addr[3];
        pre_wr_en <= addr[0] & ~we;
        write_en <= addr[0] & pre_wr_en & ~we;
        read_en <= ~(addr[0] ^ addr[1]);
      end
    end
  end
  
  assign Dxxx = (addr[15:12] == 4'b1101);

  assign aux = ( ((ram_page == 0 || ram_page == 8'h1) & ALTZP ) ||
       (((ram_page == 8'h3) | (ram_page >= 8'h8 && ram_page <= 8'h1f) | (ram_page >= 8'h40 && ram_page <= 8'hbf)) & (( RAMRD & ~we) | (RAMWRT & we))) ||
       (ram_page >= 8'h4 && ram_page <= 8'h7 && ((STORE80 & PAGE2) || (~STORE80 & (( RAMRD & ~we ) | ( RAMWRT & we))))) ||
       (ram_page >= 8'h20 && ram_page <= 8'h3f && ((STORE80 & PAGE2 & HIRES) || (~STORE80 & (( RAMRD & ~we ) | ( RAMWRT & we))))) ||
       (ram_page >= 8'hd0 & ALTZP) );

  assign ram_addr = { addr[15:13], addr[12] & ~(bank1 & Dxxx), addr[11:0]};

  assign card_ram_we = write_en;
  assign card_ram_rd = read_en;

endmodule
