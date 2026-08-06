import { render, screen } from '@testing-library/react'
import { describe, it, expect } from 'vitest'
import { ReactFlowProvider } from '@xyflow/react'
import type { ReactNode } from 'react'
import { EventNodeComponent } from './EventNode'
import { CommandNodeComponent } from './CommandNode'
import { QueryNodeComponent } from './QueryNode'
import { IntegrationNodeComponent } from './IntegrationNode'
import { UIPlaceholderNodeComponent } from './UIPlaceholderNode'

const wrapper = ({ children }: { children: ReactNode }) => (
  <ReactFlowProvider>{children}</ReactFlowProvider>
)

describe('EventNodeComponent', () => {
  it('renders the label', () => {
    render(<EventNodeComponent data={{ label: 'OrderPlaced' }} />, { wrapper })
    expect(screen.getByText('OrderPlaced')).toBeInTheDocument()
  })

  it('has orange background styling', () => {
    const { container } = render(<EventNodeComponent data={{ label: 'E' }} />, { wrapper })
    expect(container.querySelector('[data-variant="event"]')).not.toBeNull()
  })
})

describe('CommandNodeComponent', () => {
  it('renders the label', () => {
    render(<CommandNodeComponent data={{ label: 'PlaceOrder' }} />, { wrapper })
    expect(screen.getByText('PlaceOrder')).toBeInTheDocument()
  })

  it('has blue background styling', () => {
    const { container } = render(<CommandNodeComponent data={{ label: 'C' }} />, { wrapper })
    expect(container.querySelector('[data-variant="command"]')).not.toBeNull()
  })
})

describe('QueryNodeComponent', () => {
  it('renders the label', () => {
    render(<QueryNodeComponent data={{ label: 'OrderSummary' }} />, { wrapper })
    expect(screen.getByText('OrderSummary')).toBeInTheDocument()
  })

  it('has green background styling', () => {
    const { container } = render(<QueryNodeComponent data={{ label: 'Q' }} />, { wrapper })
    expect(container.querySelector('[data-variant="query"]')).not.toBeNull()
  })
})

describe('IntegrationNodeComponent', () => {
  it('renders the label', () => {
    render(<IntegrationNodeComponent data={{ label: 'SendEmail', kind: 'outbound' }} />, { wrapper })
    expect(screen.getByText('SendEmail')).toBeInTheDocument()
  })

  it('renders a cogwheel icon', () => {
    render(<IntegrationNodeComponent data={{ label: 'I', kind: 'outbound' }} />, { wrapper })
    expect(screen.getByLabelText('integration')).toBeInTheDocument()
  })

  it('has gray background styling', () => {
    const { container } = render(
      <IntegrationNodeComponent data={{ label: 'I', kind: 'inbound' }} />,
      { wrapper },
    )
    expect(container.querySelector('[data-variant="integration"]')).not.toBeNull()
  })
})

describe('UIPlaceholderNodeComponent', () => {
  it('renders the label', () => {
    render(<UIPlaceholderNodeComponent data={{ label: 'Order Form' }} />, { wrapper })
    expect(screen.getByText('Order Form')).toBeInTheDocument()
  })

  it('has dashed border styling', () => {
    const { container } = render(
      <UIPlaceholderNodeComponent data={{ label: 'UI' }} />,
      { wrapper },
    )
    expect(container.querySelector('[data-variant="uiPlaceholder"]')).not.toBeNull()
  })
})
