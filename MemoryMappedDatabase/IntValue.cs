namespace MemoryMappedDatabase
{
    public struct IntValue : IColumnValue
    {
        private readonly int m_Value;

        public IntValue(int i)
        {
            m_Value = i;
        }
    }
}